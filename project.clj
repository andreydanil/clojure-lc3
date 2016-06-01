(defproject lc-3 "0.0.1-SNAPSHOT"
  :description "An lc-3 simulator in Clojure for a compiler class."
	:dependencies [[org.clojure/clojure "1.6.0"]]
	:profiles {:dev {:dependencies [[midje "1.6.3"]]}}
	:repl-options {:init-ns lc_3.core}
)

