(defproject gd-house-prices "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [incanter/incanter "1.5.7"]
                 [clj-time "0.8.0"]]
  :resource-paths ["data"]
  :aot [gd-house-prices.core]
  :main gd-house-prices.core
  :repl-options {:init-ns gd-house-prices.examples}
  :profiles {:dev {:dependencies [[org.clojure/tools.cli "0.3.1"]]}})
