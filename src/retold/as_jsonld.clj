(ns retold.as-jsonld
  (:require [clj-yaml.core :as yaml]
            [babashka.cli :as cli]))

(def cli-options {:file {}})
(def ns1 "bts:")
(def bts "http://schema.biothings.io")

(def opts (cli/parse-opts *command-line-args* {:spec cli-options}))

(defn make-class-id [s]
  (str (str/capitalize (re-find #"^." s)) (subs (str/replace s #" " "") 1)))

(defn read-yaml [file]
  (yaml/parse-string (slurp file)))

(def dm (read-yaml (opts :file)))

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

(defn translate-required [m]
  (if (m :required)
    (assoc m "sms:required" "sms:true")
    (assoc m "sms:required" "sms:false")))

(defn which-type [t]
  (if (= "slots" t) "rdfs:Class" "rdf:Property"))

(defn as-entity [m]
  {"@id" (str ns1 id)
   "@type" (str "rdfs:Class")
   "rdfs:comment" (m :description)
   "rdfs:label" (str ns1 id)
   "rdfs:subClassOf" (into [] (m :subClassOf))
   "schema:isPartOf" {"@id" schema-uri}
   "sms:displayName" (str ns1 id)
   "sms:required" (get m "sms:required")
   "sms:validationRules" (get-in m [:annotations :validationRules] [])
   ;"sms:rangeIncludes" (get-in enums [(get m :range) :permissible_values])
   })

(defn make-graph [m]
  {"@id" "http://schema.biothings.io/#0.1"
   "@graph" (map #(as-entity %) m)})

(process-file! (opts :file))
;(process-dir! (opts :dir))
