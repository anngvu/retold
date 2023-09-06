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

(defn format-class [idx]
  {(keyword (:rdfs:label idx))
   {:is_a (first (map new-id (:rdfs:subClassOf idx)))
    :slots (map new-id (:sms:requiresDependency idx)) }})

(def class-export {:classes (into {} (map #(format-class %) classes))})

(def used-props
  (->>(mapcat :sms:requiresDependency classes)
      (map :id)
      (set)))

(def props
  "Only export props actually used"
  (filter #(contains? used-props (:id %)) g))

(defn format-prop [idx]
  {(keyword (:rdfs:label idx))
   (cond->
       {:title (:sms:displayName idx)
        :required (= "sms:required" (:sms:required idx))
        :description (:rdfs:comment idx)}
    (idx :sms:validationRules) (assoc :annotations {:validationRules (:sms:validationRules idx) })
    (idx :schema:rangeIncludes) (assoc :range (str (:rdfs:label idx) "Enum"))) })

(defn map-enum [x]
  {:id (keyword (str (new-id x) "Enum"))
   :title (:sms:displayName x)
   :values (map :id (:schema:rangeIncludes x))
   :prop (:id x)
   :domain (:id (first (:rdfs:subClassOf x)))
   })

(defn enum-stats
  "Show enums reused more than n times"
  [n]
  (filter #(> (count (:id (second %))) n) enum-sets))

(defn in? [coll x] (some #(= x %) coll))
  "Look for enums that are used"

(def enums
  (->>(filter #(> (count (:schema:rangeIncludes %)) 1) g)
      (map map-enum)
      (filter #(contains? used-props (% :prop)))))

(defn id-to-display [coll]
  (->>(filter #(in? coll (:id %)) g)
      (map :sms:displayName)))

(def enum-sets
  (->>(cset/index (set enums) [:values])
      (map (fn [[vals xs]] [vals {:id (map :id xs) :domain (first (map :domain xs))}]))))

(defn empty-key [x] {(keyword x) nil})

(defn format-enum [[vset instances]]
  {(first (:id instances))
   {:permissible_values
    (into {} (map empty-key (id-to-display (:values vset)))) }})

(defn enum-export [subset]
  (if subset
    {:enums (into {} (map #(format-enum %) (filter #(= subset (:domain (second %))) enum-sets)))}
    {:enums (into {} (map #(format-enum %) enum-sets))}))

(def prop-export {:slots (into {} (map #(format-prop %) props)) })

(defn write-yaml! [export filename]
  (spit filename (yaml/generate-string export :dumper-options {:flow-style :block})))

(doseq [e (set (map :domain enums))]
  (write-yaml! (enum-export e) (str "enums/" (str/replace e "bts:" "") ".yaml")))
