(ns retold.as-jsonld
  (:require [clojure.java.io :as io]
            [clj-yaml.core :as yaml]
            [clojure.string :as str]
            [cheshire.core :as json]))

(def default-ns "bts:")
(def bts "http://schema.biothings.io")
(def enum-db (atom nil))

(defn graph-into-context [g]
  {"@context"
   {:bts bts
    :linkml "https://w3id.org/linkml/"
    :rdf "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
    :rdfs "http://www.w3.org/2000/01/rdf-schema#"
    :schema "http://schema.org"
    :xsd "http://w3.org/2001/XMLSchema#"}
   "@id" bts
   "@graph" g})

(defn make-id [s]
  (str default-ns (str/replace s #" " "")))

(defn read-yaml [file] (yaml/parse-string (slurp file)))

(defn list-files [dir] (map str (filter #(.isFile %) (file-seq (io/file dir)))))

(defn dir-to-map [dir] (mapv read-yaml (list-files dir)))

(defn filter-type [k dm]
  (let [coll (filter #(k %) dm)]
    (if (= 1 (count coll))
      ((first coll) k)
      ((reduce #(merge-with merge %1 %2) coll) k))))

(defn get-enum [range]
  (map name (keys (get-in @enum-db [(keyword range) :permissible_values]))))

(defn id-refs [name-coll]
  (map (fn [nm] { "@id" (make-id nm) }) name-coll))

(defn sms-required [derived m]
  (if (get m :required)
    (assoc derived "sms:required" "sms:true")
    (assoc derived "sms:required" "sms:false")))

(defn expand-union-range [any_of]
  (id-refs (flatten (map #(get-enum (:range %)) any_of))))

(defn sms-range [derived m]
  (cond
    (get m :enum_range) (assoc derived "sms:rangeIncludes" (id-refs (m :enum_range)))
    (get m :any_of) (assoc derived "sms:rangeIncludes" (expand-union-range (m :any_of)))
    (get m :range) (assoc derived "sms:rangeIncludes" (id-refs (get-enum (get m :range))))
    :else (assoc derived "sms:rangeIncludes" '({"@id" "schema:Text"}))))

; this might do fancier things in the future
(defn derive-slot [derived m]
  (->(sms-range derived m)
     (assoc "sms:validationRules" (get-in m [:annotations :validationRules]))
     (assoc "rdfs:subPropertyOf" [])))

(defn derive-class [derived m]
  (->(assoc derived "sms:requiresDependency" (id-refs (m :slots)))
     (assoc "sms:requiresComponent" (get-in m [:annotations :requiresComponent]))
     (assoc "rdfs:subClassOf" (id-refs (list (get m :is_a ()))))))

(defn derive-enum [derived m]
  (->(assoc derived "rdfs:subClassOf" [])))

(defn as-entity [entity type]
  (let [label (name (entity 0)) m (val entity)]
    (cond->
      {"@id" (make-id label)
       "@type" (if (= "slots" type) "rdf:Property" "rdfs:Class")
       "rdfs:comment" (get m :description "TBD")
       "rdfs:label" label
       "schema:isPartOf" {"@id" bts}
       "sms:displayName" label}
      (= "classes" type) (derive-class m)
      ;(= "enum" type) (derive-enum m)
      (= "slots" type) (derive-slot m))))

(defn graph! [dir]
  (let [dm (dir-to-map dir)
        enums (filter-type :enums dm)
        slots (filter-type :slots dm)
        classes (filter-type :classes dm)]
    (do
      (swap! enum-db merge enums)
      (->>
       (map #(into (sorted-map) (as-entity % "enums")) enums)
       (merge (map #(into (sorted-map) (as-entity % "slots")) slots))
       (merge (map #(into (sorted-map) (as-entity % "classes")) classes))
       (flatten)
       (graph-into-context)))))

; opts should be a map { :dir "modules" :out "model.jsonld" }
(defn write-file [opts]
  (json/generate-stream (graph! (opts :dir)) (io/writer (opts :out))))
