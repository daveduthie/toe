(defproject toe "0.2.0"
  :description "FIXME: write description"
  :url "github.com/daveduthie/toe"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]]
  :main ^:skip-aot toe.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
