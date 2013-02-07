;; Back in the good old days, I liked to make clojure.contrib a dependency of my REPL.

;; And having done that, I'd start a background thread to require
;; every bit of it, so that its documentation became available to me
;; using find-doc and allied functions.

;; And life was good.

;; But then behold, a serpent is come into my garden, and sundered clojure.contrib into tiny fragments
;; and all was confusion and vexation.

;; And I went to the great spirit, and demanded "How do I depend on every clojure library?"

;; http://stackoverflow.com/questions/13673094/how-do-i-depend-on-every-clojure-contrib-library

;; And was answered by Robert P. Levy. May his tribe increase.

;; I'm paraphrasing his answer here to make it a bit clearer what's going on.

;; Robert's made it into a library, so assuming that you have the
;; invaluable pomegranate on your classpath you can just:

(require 'cemerick.pomegranate)

(cemerick.pomegranate/add-dependencies 
 :coordinates '[[rplevy/contrib-repl "0.1.2"]]
 :repositories {"clojars" "http://clojars.org/repo"})

(require 'contrib-repl.manually)

(contrib-repl.manually/add-contrib-deps)

(doseq [i contrib-repl.manually/contrib-libraries]
  (println i)
  (require (symbol (str "clojure." i)) :verbose))

(find-doc "mona")


(contrib-repl.manually/add-contrib-deps ["data.codec"])

;;hmm, clojure.data.codec breaks massive require!
(require 'clojure.data.codec.base64)


(require '[cemerick.pomegranate :refer [add-dependencies]])

(add-dependencies
  :coordinates '[[clj-http "0.5.8"]]
  :repositories {"clojars" "http://clojars.org/repo"})

(require '[clj-http.client :as client])

(def contrib ["tools.nrepl" "tools.trace" "tools.namespace" "tools.macro"
              "test.generative" "math.numeric-tower" "core.match" "core.logic"
              "data.priority-map" "core.contracts" "tools.cli" "java.jmx"
              "java.jdbc" "java.classpath" "data.xml" "data.json" "core.unify"
              "core.incubator" "core.cache" "algo.monads" "data.generators"
              "core.memoize" "math.combinatorics" "java.data" "tools.logging"
              "data.zip" "data.csv" "algo.generic" "data.codec"
              "data.finger-tree"])


(defn add-contrib-dependencies
  "look up the latest version of every contrib project in maven central,
   and add them as dependencies using pomegranate."
  [project-names]
  (add-dependencies
   :coordinates
   (map (juxt
         (comp symbol (partial format "org.clojure/%s"))
         (fn [proj]
             (Thread/sleep 100)
             (-> "http://search.maven.org/solrsearch/select?q=%s&rows=1&wt=json"
                 (format proj)
                 (client/get {:as :json})
                 :body :response :docs first :latestVersion)))
        project-names)))


(let [project-names '(java.jdbc)]
  (map (juxt
        (comp symbol (partial format "org.clojure/%s"))
        (fn [proj]
          (Thread/sleep 100)
          (-> "http://search.maven.org/solrsearch/select?q=%s&rows=1&wt=json"
              (format proj)
              (client/get {:as :json})
              :body :response :docs first :latestVersion)))
       project-names))



((fn [proj]
          (Thread/sleep 100)
          (-> "http://search.maven.org/solrsearch/select?q=%s&rows=1&wt=json"
              (format proj)
              (client/get {:as :json})
              :body :response :docs first :latestVersion))
 "java.jdbc")


(client/get (format "http://search.maven.org/solrsearch/select?q=%s&rows=1&wt=json" "java.jdbc") {:as :json})
