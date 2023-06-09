(ns retold.as-jsonld
  (:require [clojure.java.io :as io]
            [clj-yaml.core :as yaml]
            [clojure.string :as str]
            [cheshire.core :as json]))

(def default-ns "bts:")
(def bts "http://schema.biothings.io/")
(def graph (atom nil))

(defn graph-with-context [g]
  {"@context"
   {:bts bts
    :linkml "https://w3id.org/linkml/"
    :rdf "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
    :rdfs "http://www.w3.org/2000/01/rdf-schema#"
    :schema "http://schema.org/"
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
      ((apply merge-with merge coll) k))))

(defn get-enum [range]
  (map name (keys (get-in @graph [:enums (keyword range) :permissible_values]))))

(defn id-refs [name-coll]
  (map (fn [nm] { "@id" (make-id nm) }) name-coll))

(defn sms-required [derived m]
  (if (get m :required)
    (assoc derived "sms:required" "sms:true")
    (assoc derived "sms:required" "sms:false")))

(defn expand-union-range [any_of]
  (id-refs (flatten (map #(get-enum (:range %)) any_of))))

; range allows anonymous inlined enum vals, update graph
(defn sms-range [derived m]
  (cond
    (get m :enum_range) (assoc derived "schema:rangeIncludes" (id-refs (m :enum_range)))
    (get m :any_of) (assoc derived "schema:rangeIncludes" (expand-union-range (m :any_of)))
    (get m :range) (assoc derived "schema:rangeIncludes" (id-refs (get-enum (get m :range))))
    :else derived))

(defn derive-slot [derived m]
  (let [vrules (get-in m [:annotations :validationRules])]
    (cond->
        (sms-range derived m)
        true (sms-required m)
        (str/blank? vrules) (assoc "sms:validationRules" [])
        (not (str/blank? vrules)) (assoc "sms:validationRules" (list vrules))
        (get m :domain) (assoc "schema:domainIncludes" { "@id" (make-id (m :domain)) })
        true (assoc "rdfs:subPropertyOf" []))))

(defn derive-class [derived m]
  (->(assoc derived "sms:requiresDependency" (id-refs (m :slots)))
     (assoc "sms:requiresComponent" (get-in m [:annotations :requiresComponent]))
     (assoc "rdfs:subClassOf" (id-refs (list (get m :is_a ()))))))

(defn derive-enum [derived m]
  (sms-required derived m))

(defn as-entity [entity type]
  (let [label (name (entity 0)) m (val entity)]
    (cond->
      {"@id" (make-id label)
       "@type" "rdfs:Class";(if (= "slots" type) "linkml:SlotDefinition" "rdfs:Class")
       "rdfs:comment" (get m :description "TBD")
       "rdfs:label" (str/replace label #" " "")
       "schema:isPartOf" {"@id" bts}
       "sms:displayName" label}
      (= "vals" type) (assoc "sms:required" "sms:false")
      (= "classes" type) (derive-class m)
      (= "enums" type) (derive-enum m)
      (= "slots" type) (derive-slot m))))

(defn graph! [dir]
  (let [dm (dir-to-map dir)
        enums (filter-type :enums dm)
        vals (apply merge (map #((val %) :permissible_values) enums))
        slots (filter-type :slots dm)
        classes (filter-type :classes dm)]
    (do
      (swap! graph merge {:enums enums :vals vals :slots slots :classes classes})
      (->>
       (map #(into (sorted-map) (as-entity % "enums")) enums)
       (merge (map #(into (sorted-map) (as-entity % "vals")) vals))
       (merge (map #(into (sorted-map) (as-entity % "slots")) slots))
       (merge (map #(into (sorted-map) (as-entity % "classes")) classes))
       (flatten)
       (graph-with-context)))))

; opts should be a map { :dir "modules" :out "model.jsonld" }
(defn write-file [opts]
  (json/generate-stream (graph! (opts :dir)) (io/writer (opts :out)) {:pretty true})
  (println "Exported " (opts :out)))
