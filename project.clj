(defproject project-euler-clojure "0.1.0"
  :description "My Solutions to Project Euler with Clojure"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/math.combinatorics "0.1.3"]]
  :main ^:skip-aot project-euler.core
  :target-path "target/%s"
  ;; :jvm-opts ["-Xss10M"] ;; (for problem 0094)
  :profiles {:uberjar {:aot :all}})
