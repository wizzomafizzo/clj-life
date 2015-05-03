(defproject clj-life "0.1.0-SNAPSHOT"
  :description "Conway's Game of Life"
  :url "https://github.com/wizzomafizzo/clj-life"
  :license {:name "The MIT License"}
  :dependencies [[org.clojure/clojure "1.7.0-beta2"]
                 [quil "2.2.5"]]
  :main ^:skip-aot clj-life.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
