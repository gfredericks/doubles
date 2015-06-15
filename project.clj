(defproject com.gfredericks/doubles "0.1.0-SNAPSHOT"
  :description "Some stuff about doubles"
  :url "https://github.com/gfredericks/doubles"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0-RC1"]]
  :profiles
  {:dev {:dependencies [[org.clojure/test.check "0.8.0-alpha3"]
                        [com.gfredericks/test.chuck "0.1.18"]]}})
