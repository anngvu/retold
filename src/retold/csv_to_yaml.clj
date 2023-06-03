(ns retold.csv_to_yaml
  (:require [clojure.java.io :as io]
            [clojure.data.csv :as csv]
            [clojure.string :as str]
            [clj-yaml.core :as yaml]))

(defn read-terms [file]
  (with-open [reader (io/reader file)]
    (let [rows (csv/read-csv reader)]
      (mapv #(zipmap (first rows) %) (rest rows)))))

(defn to-enum-range [derived p]
  (assoc derived :enum_range (map str/trim (str/split (get p "Valid Values") #","))))

(defn format-prop [p]
  { (keyword (get p "Attribute"))
   (cond->
       {:description (get p "Description")
        :required (= 'TRUE' (get p "Required"))
        :annotations { :requiresDependency (get p "DependsOn") :validationRules (get p "Validation Rules") }
        }
     (not (str/blank? (get p ".Range"))) (assoc :range (get p ".Range"))
     (and (str/blank? (get p ".Range")) (not (str/blank? (get p "Valid Values")))) (to-enum-range p))
   })

(defn format-enum [e]
  {:parent (get e "Parent" "Other") :attr (get e "Attribute") :data
   (cond->
       {:description (get e "Description")}
     (not (str/blank? (get e "Source"))) (assoc :meaning (get e "Source"))
     (not (str/blank? (get e ".EditorNote"))) (assoc :notes (get e ".EditorNote")))
   })

(defn format-class [t]
  { (keyword (get t "Attribute"))
   (cond->
       {:description (get t "Description")
        :slots (str/split (get t "DependsOn" "") #",")
        :annotations
        {:requiresComponent (get t "DependsOn Component")
         :required (= 'TRUE' (get t "Required")) }
        }
     (not (str/blank? (get t ".EditorNote"))) (assoc :notes (get t ".EditorNote")))
   })

(defn output-prop [props]
  {:slots (into {} (map #(format-prop %) props)) })

(defn output-enum [enums]
  (->>
   (map #(format-enum %) enums)
   (group-by :parent)
   (map (fn [m] { (m 0) { :permissible_values (into {} (map (fn [v] { (keyword (v :attr)) (v :data) }) (m 1))) }}))
   (into {})
   (assoc {:enums nil} :enums)
   ))

(defn output-class [classes]
  {:classes (into {} (map #(format-class %) classes))})

(defn add-id [file namespace data]
  (let [name (re-find #"[\w-]+?(?=\.)" file)]
    (merge {:id (str namespace name)
            :name name
            :default_range "string" } data)))

(defn transform [file namespace]
  (let [terms (read-terms file)]
    (->>
     (cond
       (= "annotationProperty" (re-find #"annotationProperty" file)) (output-prop terms)
       (= "Template" (re-find #"Template" file)) (output-class terms)
       :else (output-enum terms))
     ;(add-id namespace file)
     )))

(defn process-file! [file]
  (spit (str/replace file #"csv" "yaml")
        (yaml/generate-string (transform file) :dumper-options {:flow-style :block})))

(defn process-dir! [dir]
  (doseq [file (map str (filter #(.isFile %) (file-seq (io/file dir))))]
    (print (str file "\n"))
    (process-file! file)))
