(ns retold.as-jsonld
  (:require [clojure.java.io :as io]
            [clj-yaml.core :as yaml]
            [babashka.cli :as cli]))

(def cli-options {:file {}})
(def ns1 "bts:")
(def bts "http://schema.biothings.io")

(def opts (cli/parse-opts *command-line-args* {:spec cli-options}))

(defn make-class-id [s]
  (str (str/capitalize (re-find #"^." s)) (subs (str/replace s #" " "") 1)))

(defn read-yaml [file]
  (yaml/parse-string (slurp file)))

;(def dm (read-yaml (opts :file)))

(def slots (filter #(:slots %) dm))
(def classes (filter #(:class %) dm))
(def enums (filter #(:enums %) dm))
(def default-context
  {"@context"
   {:bts bts
    :rdf "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
    :rdfs "http://www.w3.org/2000/01/rdf-schema#"
    :schema "http://schema.org"
    :xsd "http://w3.org/2001/XMLSchema#"}})

(defn sms-required [value m]
  (if (value)
    (assoc m "sms:required" "sms:true")
    (assoc m "sms:required" "sms:false")))

(defn sms-range [value m]
  (assoc m "sms:rangeIncludes" :range value))

; this might do fancier things in the future
(defn sms-validation-rules [value m]
  (assoc m "sms:validationRules" value))

(defn which-type [t]
  (if (= "slots" t) "rdfs:Class" "rdf:Property"))

(defn get-enum [range]
  (get-in enums [(keyword range) :permissible_values]))

(defn as-entity [m type]
  (cond->
      {"@id" (str ns1 id)
       "@type" (str "rdfs:Class")
       "rdfs:comment" (m :description)
       "rdfs:label" (str ns1 id)
       "rdfs:subClassOf" (into [] (m :subClassOf))
       "schema:isPartOf" {"@id" schema-uri}
       "sms:displayName" (str ns1 id)}
       ((= "classes" type) (sms-required (get-in m [:annotations :required])))
       ((= "enums" type) (sms-required (get m :required)))
       ((= "slots" type) (sms-range (get-enum (get m :range))))
       ((= "slots" type) (sms-validation-rules (get-in m [:annotations :validationRules]))))
   )

(defn make-graph [m]
  {"@id" "http://schema.biothings.io/#0.1"
   "@graph" (map #(as-entity %) m)})

;(process-file! (opts :file))
;(process-dir! (opts :dir))
