#!/usr/bin/env bb

(require '[babashka.cli :as cli])
(require '[retold.as_jsonld :as as-jsonld])
(require '[retold.csv_to_yaml :as as-yaml])

(def cli-opts
  {:file     {:alias   :f
               :desc    "File to translate"
               :require true}
   :dir      {:alias   :d
              :desc    "Directory of files to translate"}})

(def table
  [{:cmds ["as-jsonld"] :fn as-jsonld/translate :spec cli-opts}
   {:cmds ["as-yaml"] :fn as-yaml/translate :spec cli-opts}])

(cli/dispatch table *command-line-args*)
