(defproject hlcup "0.1.0-SNAPSHOT"

  :description "FIXME: write description"

  :url "http://example.com/FIXME"

  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[org.clojure/clojure "1.9.0"]

                 [aleph "0.4.6"]
                 [ring/ring-core "1.7.1"]
                 [compojure "1.6.1"]
                 [ring/ring-json "0.4.0"]
                 [mount "0.1.15"]
                 [cheshire "5.8.1"]
                 [clj-time "0.15.0"]
                 [com.datomic/datomic-free "0.9.5697"]


                 [org.clojure/data.csv "0.1.4"]
                 [clj-http "3.9.1"]

                 ]

  :main ^:skip-aot hlcup.core

  :target-path "target/%s"

  :profiles {:uberjar {:aot :all}})
