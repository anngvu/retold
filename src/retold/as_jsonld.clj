(ns retold.as-jsonld
  (:require [clojure.java.io :as io]
            [clj-yaml.core :as yaml]
            [clojure.string :as str]
            [jsonista.core :as json]
            [babashka.cli :as cli]))

(def cli-options {:file {}})
(def default-ns "bts:")
(def bts "http://schema.biothings.io")

(def opts (cli/parse-opts *command-line-args* {:spec cli-options}))

(defn make-class-id [s]
  (str (str/capitalize (re-find #"^." s)) (subs (str/replace s #" " "") 1)))

(defn read-yaml [file]
  (yaml/parse-string (slurp file)))

(defn list-files [dir]
  (map str (filter #(.isFile %) (file-seq (io/file dir)))))

(defn files-into-map [files] (mapv read-yaml files))

(defn filter-type [k]
  (let [coll (filter #(k %) dm)]
    (if (= 1 (count coll))
      ((first coll) k)
      (reduce #(merge (%1 k) (%2 k)) coll))))

(def dm (files-into-map (list-files "data")))
(def slots (filter-type :slots))
(def classes (filter-type :classes))
(def enums (filter-type :enums))

(def default-context
  {"@context"
   {:bts bts
    :rdf "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
    :rdfs "http://www.w3.org/2000/01/rdf-schema#"
    :schema "http://schema.org"
    :xsd "http://w3.org/2001/XMLSchema#"}})

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

(defn get-enum [range]
  (get-in enums [(keyword range) :permissible_values]))

(defn as-entity [entity type]
  (let [label (name (entity 0)) m (val entity)]
    (cond->
      {"@id" (str default-ns (make-class-id label))
       "@type" (if (= "slots" type) "rdf:Property" "rdfs:Class")
       "rdfs:comment" (m :description)
       "rdfs:label" label
       "rdfs:subClassOf" (into [] (m :subClassOf))
       "schema:isPartOf" {"@id" bts}
       "sms:displayName" label}
       (= "classes" type) (sms-required m)
       (= "enums" type) (sms-required m)
       (= "slots" type) (sms-range m)
       (= "slots" type) (sms-validation-rules m)
       )))

(def enum-graph (map #(as-entity % "enums") enums))
(def prop-graph (map #(as-entity % "slots") slots))
(def class-graph (map #(as-entity % "classes") classes))

(def graph
  {"@id" bts
   "@graph" (merge enum-graph prop-graph class-graph)})

(def json-file (java.io.File. "model.jsonld"))

(json/write-value json-file graph)
