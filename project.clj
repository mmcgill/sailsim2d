(defproject sailsim2d "0.1.0-SNAPSHOT"
  :description "2D multiplayer sailing game "
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/core.async "0.1.346.0-17112a-alpha"]
                 [org.clojure/data.json "0.2.5"]
                 [http-kit "2.1.16"]
                 [compojure "1.3.1"]
                 [hiccup "1.0.5"]
                 [prismatic/schema "0.4.4-mmcgill"]]
  :plugins [[lein-cloverage "1.0.6"]]
  :profiles {:dev {:dependencies [[net.mikera/imagez "0.6.0"]]}})
