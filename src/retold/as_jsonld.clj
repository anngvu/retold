(ns retold.as-jsonld
  (:require [clojure.java.io :as io]
            [clj-yaml.core :as yaml]
            [clojure.string :as str]
            [cheshire.core :as json]))

(def default-ns "bts:")
(def bts "http://schema.biothings.io/")
(def graph (atom nil))

(defn with-context [g]
  {"@context"
   {:bts bts
    :linkml "https://w3id.org/linkml/"
    :rdf "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
    :rdfs "http://www.w3.org/2000/01/rdf-schema#"
    :schema "http://schema.org/"
    :xsd "http://w3.org/2001/XMLSchema#"}
   "@id" bts
   "@graph" g})

(def typemap {:classes :class :enums :enum :slots :slot})

(defn make-id [s] (str default-ns (str/replace s #" " "")))

(defn read-yaml [file] (yaml/parse-string (slurp file)))

(defn list-files [dir] (map str (filter #(.isFile %) (file-seq (io/file dir)))))

(defn type-children "Add type to a collection of child entities"
  [children type]
  (reduce-kv (fn [m k v] (assoc m k (assoc v :type type))) {} children))

(defn dir-to-map [dir]
  (->>(mapv read-yaml (list-files dir))
      (apply merge-with merge)
      (reduce-kv (fn [m k v] (assoc m k (type-children v (k typemap)))) {})))

(defn get-enum [range]
  (map name (keys (get-in @graph [:enums (keyword range) :permissible_values]))))

(defn id-refs [name-coll]
  (map (fn [nm] { "@id" (make-id nm) }) name-coll))

(defn sms-required [derived entity]
  (let [[_ props] entity]
    (if (get props :required)
      (assoc derived "sms:required" "sms:true") (assoc derived "sms:required" "sms:false"))))

(defn expand-union-range [any_of]
  (id-refs (flatten (map #(get-enum (:range %)) any_of))))

(defn sms-range [derived entity]
  (let [[_ props] entity]
    (cond
      (get props :enum_range) (assoc derived "schema:rangeIncludes" (id-refs (props :enum_range)))
      (get props :any_of) (assoc derived "schema:rangeIncludes" (expand-union-range (props :any_of)))
      (get props :range) (assoc derived "schema:rangeIncludes" (id-refs (get-enum (props :range))))
      :else derived)))

(defn base-entity [entity]
  (let [[k props] entity]
    {"@id" (make-id (name k))
     "@type" "rdfs:Class"
     "rdfs:comment" (get props :description "TBD")
     "rdfs:label" (str/replace (name k) #" " "")
     "rdfs:subClassOf" ()
     "schema:isPartOf" {"@id" bts}
     "sms:displayName" (name k)
     "sms:required" "sms:false"}))
      
(defmulti derive-entity (fn [entity] (let [[_ props] entity] (get props :type))))

(defmethod derive-entity :default [entity] (base-entity entity))

(defmethod derive-entity :class [entity]
  (let [[_ props] entity]
    (->(base-entity entity)
       (assoc "sms:requiresDependency" (id-refs (get props :slots)))
       (assoc "sms:requiresComponent" (get-in props [:annotations :requiresComponent]))
       (assoc "rdfs:subClassOf" (id-refs (if-let [subclass (get props :is_a)] (list subclass) ()))))))

; TODO When schematic bug is fixed, override "@type" with "rdf:Property"
(defmethod derive-entity :slot [entity]
  (let [valrules (get-in entity [:annotations :validationRules] [])]
    (->(base-entity entity)
       (sms-range entity)
       (sms-required entity)
       (assoc "sms:validationRules" valrules))))

(defn to-vals [g]
  (->>(apply merge (map #((val %) :permissible_values) (g :enums)))
      (map (fn [m] (let [[k v] m] [(name k) v]))))) ; deal w/ spaces in vals

(defn graph-map "Create graph given source directory, realizing vals as needed"
  [dir]
  (let [g (dir-to-map dir) vals (to-vals g)]
    (->>(clojure.set/difference (set (mapcat #((val %) :enum_range) (g :slots))) (set (mapv first vals)))
        (map (fn [v] [v {}]))
        (into vals)
        (assoc g :vals))))

(defn output-graph [g] (with-context (map derive-entity (mapcat val g))))

(defn write-file [opts]
  (let [{:keys [dir out]} opts]
    (swap! graph merge (graph-map dir))
    (json/generate-stream (output-graph @graph) (io/writer out) {:pretty true})
    (println (str "Exported to " out "!"))))
