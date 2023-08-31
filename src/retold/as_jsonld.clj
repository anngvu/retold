(ns retold.as-jsonld
  (:require [clojure.java.io :as io]
            [clj-yaml.core :as yaml]
            [clojure.string :as str]
            [cheshire.core :as json]
            [clojure.set :as cset]))

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

; Be careful with key entries that include "/"
(defn key-fn [x]
  (let [k (x :key)]
    (if (str/includes? k "/") k (keyword k))))

(defn read-yaml [file] (yaml/parse-string (slurp file) :key-fn key-fn))

(defn list-files [dir] (map str (filter #(.isFile %) (file-seq (io/file dir)))))

(defn type-children "Add/pass down type to elements in collection using typemap"
  [children type]
  (reduce-kv (fn [m k v] (assoc m k (assoc v :type type))) {} children))

(defn dir-to-map [dir]
  (->>(mapv read-yaml (list-files dir))
      (apply merge-with merge)
      (reduce-kv (fn [m k v] (assoc m k (type-children v (k typemap)))) {})))

(defn get-enum "Use range reference to get the set of valid enum values"
  [range]
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

(defn sms-deps [derived entity]
  (let [[_ props] entity
        deps (get-in props [:annotations :requiresDependency])]
    (if deps (assoc derived "sms:requiresDependency" (id-refs (str/split deps #","))) derived)))

(defn sms-rules [derived entity]
  (let [[_ props] entity]
    (if-let [rules (get-in props [:annotations :validationRules])]
      (assoc derived "sms:validationRules" (list rules)) derived)))

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
  (->(base-entity entity)
     (sms-range entity)
     (sms-required entity)
     (sms-deps entity)
     (sms-rules entity)))

(defn get-vals [g]
  (->>(apply merge (map #((val %) :permissible_values) (g :enums)))
      (map (fn [m] (let [[k v] m] [(name k) v])))
      (mapv first))) ; deal w/ spaces in vals

(defn add-vals [g vals]
  (->>(cset/difference (set (mapcat #((val %) :enum_range) (g :slots))) (set (get-vals g)))
      (map (fn [v] [v {}]))
      (into vals)
      (assoc g :vals)))

(defn class-lineage
  "Return the main class lineage/backbone for class as class -> parent -> ancestor ..."
  [class class-map]
  (loop [class-id (keyword class)
         lineage [class]]
    (let [next (get class-map class-id)]
      (if (nil? next)
        lineage
        (recur (:is_a next) (conj lineage (:is_a next)))))))

(defn inherited-props "Get inherited props according to class lineage"
  [class class-map]
  (let [[id props] class
        lineage (reverse (mapv keyword (class-lineage id class-map)))] ; reverse so inheriteds appear first
    (distinct (mapcat #(:slots (second %)) (select-keys class-map lineage)))))

(defn subclass [class class-map]
  (assoc-in class [1 :slots] (inherited-props class class-map)))

(defn graph-map "Build graph from source directory, realizing values from slots and inherited slots"
  [dir]
  (let [g (dir-to-map dir)
        classes (g :classes)
        classes-x (map #(subclass % classes)  classes)]
    (->(add-vals g (get-vals g))
       (assoc        :classes classes-x))))

(defn output-graph [g]
  (with-context (map derive-entity (mapcat val g))))

(defn write-file [opts]
  (let [{:keys [dir out]} opts]
    (swap! graph merge (graph-map dir))
    (json/generate-stream (output-graph @graph) (io/writer out) {:pretty true})
    (println (str "Exported to " out "!"))))
