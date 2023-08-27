(ns retold.as-linkml
  (:require [clojure.java.io :as io]
            [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.set :as cset]
            [clj-yaml.core :as yaml]))

; Convert JSON-LD to LinkML based on some rules of thumb:
; - set of terms in rangeIncludes may be enums
; - terms that include Component in dependsOn are classes
; - props are more reliably identified as objects of dependsOn;
; - props with rangeIncludes -> constrained, without -> free text

(defn ok-key [k] (keyword (str/replace k #"@" "")))

(defn new-id [x]
  (str/replace (:id x) "bts:" ""))

(def jsonld (json/parse-string (slurp "GF.jsonld") ok-key))

(def g (jsonld :graph))

(def classes (filter #(not (nil? (:sms:requiresDependency %))) g))

(def class-export {:classes (into {} (map #(format-class %) classes))})

(defn format-class [idx]
  {(keyword (:rdfs:label idx))
   {:is_a (first (map new-id (:rdfs:subClassOf idx)))
    :slots (map new-id (:sms:requiresDependency idx)) }})

(defn map-enum [x]
  {:id (keyword (str (new-id x) "Enum"))
   :values (map new-id (:schema:rangeIncludes x)) })

(def enums
  (->>(filter #(> (count (:schema:rangeIncludes %)) 1) g)
      (map map-enum)))

; Dedup enums
(def enum-sets
  (->>(cset/index (set enums) [:values])
      (map (fn [[vals ids]] [vals {:id (map :id ids)}]))))

; Check re-used enums
;(filter #(> (count (second %)) 5) enum-sets)

(defn format-enum [idx]
  (let [[vset instances] idx]
    {(first (:id instances))
     {:permissible_values
      (into {} (map (fn [v] {(keyword v) ""})(:values def)))}}))

(def enum-export {:enums (into {} (map #(format-enum %) enum-sets)) })

; Only export props actually used
(def used
  (->>(mapcat :sms:requiresDependency classes)
      (map :id)
      (set)))

(def props (filter #(contains? used (:id %)) g))

(defn format-prop [idx]
  {(keyword (:rdfs:label idx))
   (cond->
       {:title (:sms:displayName idx)
        :required (= "sms:required" (:sms:required idx))
        :description (:rdfs:comment idx)}
    (idx :sms:validationRules) (assoc :annotations {:validationRules (:sms:validationRules idx) })
    (idx :schema:rangeIncludes) (assoc :range (str (:rdfs:label idx) "Enum"))) })

(def prop-export {:slots (into {} (map #(format-prop %) props)) })

(defn write-yaml! [export filename]
  (spit filename (yaml/generate-string export :dumper-options {:flow-style :block})))
