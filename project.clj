(defproject retold "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.0"]
  		 [clj-commons/clj-yaml "1.0.26"]
                 [cheshire "5.11.0"]
                 [org.babashka/cli "0.7.51"]]
  ;:main retold.core

  ;; add AOT compilation
  :profiles {:uberjar {:aot :all}}
  :repl-options {:init-ns retold.core})
