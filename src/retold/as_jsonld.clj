(ns retold.as-jsonld
  (:require [clojure.java.io :as io]
            [clj-yaml.core :as yaml]
            [clojure.string :as str]
            [cheshire.core :as json]))

(def default-ns "bts:")
(def bts "http://schema.biothings.io")
(def default-context
  {"@context"
   {:bts bts
    :rdf "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
    :rdfs "http://www.w3.org/2000/01/rdf-schema#"
    :schema "http://schema.org"
    :xsd "http://w3.org/2001/XMLSchema#"}})

(defn make-class-id [s]
  (str default-ns (str/capitalize (re-find #"^." s)) (subs (str/replace s #" " "") 1)))

(defn read-yaml [file]
  (yaml/parse-string (slurp file)))

(defn list-files [dir]
  (map str (filter #(.isFile %) (file-seq (io/file dir)))))

(defn files-into-map [files] (mapv read-yaml files))

(defn filter-type [k dm]
  (let [coll (filter #(k %) dm)]
    (if (= 1 (count coll))
      ((first coll) k)
      (reduce #(merge (%1 k) (%2 k)) coll))))

(defn get-enum [range enums]
  (get-in enums [(keyword range) :permissible_values]))

(defn sms-required [derived m]
  (if (get m :required)
    (assoc derived "sms:required" "sms:true")
    (assoc derived "sms:required" "sms:false")))

(defn sms-range [derived m]
  (if (get m :enum_range)
    (assoc derived "sms:rangeIncludes" (get m :enum_range))
    (assoc derived "sms:rangeIncludes" (get-enum (get m :range)))))

; this might do fancier things in the future
(defn sms-validation-rules [derived m]
  (assoc derived "sms:validationRules" (get-in m [:annotations :validationRules])))

(defn sms-requires-component [derived m]
  (assoc derived "sms:requiresComponent" (get-in m [:annotations :requiresComponent])))

(defn as-entity [entity type]
  (let [label (name (entity 0)) m (val entity)]
    (cond->
      {"@id" (make-class-id label)
       "@type" (if (= "slots" type) "rdf:Property" "rdfs:Class")
       "rdfs:comment" (m :description)
       "rdfs:label" label
       "rdfs:subClassOf" (into [] (m :subClassOf))
       "schema:isPartOf" {"@id" bts}
       "sms:displayName" label}
       (= "classes" type) (sms-requires-component m)
       (= "slots" type) (sms-required m)
       (= "slots" type) (sms-range m)
       (= "slots" type) (sms-validation-rules m)
       )))

(defn map-graph [dir]
  (let [dm (files-into-map (list-files dir))
        enums (filter-type :enums dm)]
    (->>
     (map #(as-entity % "enums") enums)
     (merge (map #(as-entity % "slots") (filter-type :slots dm)))
     (merge (map #(as-entity % "classes") (filter-type :classes dm)))
     (assoc {"@id" bts} "@graph")
     )))

; opts should be a map { :dir "modules" :outfile "model.jsonld" }
(defn write-file [opts]
  (let [graph (map-graph (opts :dir))]
    (json/generate-stream graph (io/writer "model.jsonld"))))
