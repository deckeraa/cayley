(defproject cayley "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/math.combinatorics "0.0.7"]
                 [org.clojure/math.numeric-tower "0.0.4"]
                 [compojure "1.1.6"]
                 [hiccup "1.0.5"]
                 [garden "1.1.5"]]
  :plugins [[lein-ring "0.8.10"]
            [lein-garden "0.1.8"]]
  :ring {:handler cayley.handler/app}
  :garden {:builds [{;; Optional name of the build:
                     :id "screen"
                     ;; The var containing your stylesheet:
                     :stylesheet cayley.css/screen
                     ;; Compiler flags passed to `garden.core/css`:
                     :compiler {;; Where to save the file:
                                :output-to "resources/css/group.css"
                                ;; Compress the output?
                                :pretty-print? true}}]}
  :profiles
  {:dev {:dependencies [[javax.servlet/servlet-api "2.5"]
                        [ring-mock "0.1.5"]]}})
