;;  Friend: Authentication in a Ring App

(def dependencies '[[ring/ring "1.5.0"]
                    [com.cemerick/friend "0.2.3"]])

;; dependency gibberish
(require 'cemerick.pomegranate)
(cemerick.pomegranate/add-dependencies 
 :coordinates dependencies
 :repositories (merge cemerick.pomegranate.aether/maven-central
                      {"clojars" "http://clojars.org/repo"}))

(require 'ring.middleware.stacktrace)
(require 'ring.middleware.params)
(require 'ring.middleware.keyword-params)
(require 'ring.middleware.nested-params)
(require 'ring.adapter.jetty)
(require '[cemerick.friend :as friend])
(require '[cemerick.friend.credentials :as creds])
(require '[cemerick.friend.workflows :as workflows])


;; Some infrastructure


(defn html-escape [string] 
  (str "<pre>" (clojure.string/escape string {\< "&lt;", \> "&gt;"}) "</pre>"))

(defn wrap-spy [handler spyname include-body]
  (fn [request]
    (let [incoming (with-out-str
                     (println "-------------------------------")
                     (println spyname ":\n Incoming Request:")
                     (clojure.pprint/pprint request))]
      (println incoming)
      (let [response (handler request)]
        (let [outgoing (with-out-str 
                         (println spyname ":\n Outgoing Response Map:")
                         (clojure.pprint/pprint (if include-body response
                                                  (assoc response :body "#<?>")))
                         (println "-------------------------------"))]
          (println outgoing)
          (update-in response  [:body] (fn[x] (str (html-escape incoming) x  (html-escape outgoing)))))))))

(declare handler)
(declare app)
(declare users)

(def app
  (-> #'handler
      (wrap-spy "what the handler sees" true)
      (friend/authenticate {:credential-fn (partial creds/bcrypt-credential-fn users)
                            :workflows [(workflows/interactive-form)]})
      (ring.middleware.stacktrace/wrap-stacktrace)
      (ring.middleware.params/wrap-params)
      (ring.middleware.keyword-params/wrap-keyword-params)
      (ring.middleware.nested-params/wrap-nested-params)

      ;(wrap-spy "what the web server sees" false)
      ))


(defonce server (ring.adapter.jetty/run-jetty #'app {:port 8080 :join? false}))


;; Here's the actual app

(def users {"root" {:username "root"
                    :password (creds/hash-bcrypt "admin_password")
                    :roles #{::admin}}
            "jane" {:username "jane"
                    :password (creds/hash-bcrypt "user_password")
                    :roles #{::user}}})



(defn page1 [request]
    {:status 200
     :headers {"Content-Type""text/html"}
     :body (str  "<h1>Page One</h1>")})

(defn page2 [request]
    {:status 200
     :headers {"Content-Type""text/html"}
     :body (str  "<h1>Page Two</h1>")})

(defn login [request]
    {:status 200
     :headers {"Content-Type""text/html"}
     :body (str  "<h1>Login</h1>")})


(defn handler [request]
  (case (request :uri)
    "/page1" (friend/authorize #{::user} (page1 request))
    "/page2" (page2 request)
    "/login" (login request)
    (page2 request)))



;; Below here is messing about
;; requests can be made programmatically, of course
(app {}) ; {:status 200, :headers {"Content-Type" "text/html"}, :body "<pre>-------------------------------\nwhat the handler sees :\n Incoming Request:\n{:cemerick.friend/auth-config\n {:default-landing-uri \"/\",\n  :login-uri \"/login\",\n  :credential-fn #function[clojure.core/partial/fn--4759],\n  :workflows\n  [#function[cemerick.friend.workflows/interactive-form/fn--12166]]}}\n</pre><h1>Page Two</h1><pre>what the handler sees :\n Outgoing Response Map:\n{:status 200,\n :headers {\"Content-Type\" \"text/html\"},\n :body \"&lt;h1&gt;Page Two&lt;/h1&gt;\"}\n-------------------------------\n</pre>"}
(app {:uri "/page2"}) ; {:status 200, :headers {"Content-Type" "text/html"}, :body "<pre>-------------------------------\nwhat the handler sees :\n Incoming Request:\n{:uri \"/page2\",\n :cemerick.friend/auth-config\n {:default-landing-uri \"/\",\n  :login-uri \"/login\",\n  :credential-fn #function[clojure.core/partial/fn--4759],\n  :workflows\n  [#function[cemerick.friend.workflows/interactive-form/fn--12166]]}}\n</pre><h1>Page Two</h1><pre>what the handler sees :\n Outgoing Response Map:\n{:status 200,\n :headers {\"Content-Type\" \"text/html\"},\n :body \"&lt;h1&gt;Page Two&lt;/h1&gt;\"}\n-------------------------------\n</pre>"}
(app {:uri "/page1"}) ; {:status 200, :headers {"Content-Type" "text/html"}, :body "<pre>-------------------------------\nwhat the handler sees :\n Incoming Request:\n{:uri \"/page1\",\n :cemerick.friend/auth-config\n {:default-landing-uri \"/\",\n  :login-uri \"/login\",\n  :credential-fn #function[clojure.core/partial/fn--4759],\n  :workflows\n  [#function[cemerick.friend.workflows/interactive-form/fn--12166]]}}\n</pre><h1>Page One</h1><pre>what the handler sees :\n Outgoing Response Map:\n{:status 200,\n :headers {\"Content-Type\" \"text/html\"},\n :body \"&lt;h1&gt;Page One&lt;/h1&gt;\"}\n-------------------------------\n</pre>"}

(users "root") ; {:username "root", :password "$2a$10$La6g7yBmLLZbKZ/a29j/Uumf2mpFdhimrncK1FS/bP/uqkLMSG89y", :roles #{:user/admin}}
(users "jane") ; {:username "jane", :password "$2a$10$H/b/UF1KLSGTI/dcolGOyOPZXSSyL5RKJD1OkhezKDfhMnBBaJTNO", :roles #{:user/user}}

(creds/hash-bcrypt "user-password")  ; "$2a$10$zsuc3oNkVA1/a8dIwFSVQeFq73GC7st2bstx1oDsMa3MMr8.SVEjS"
(creds/hash-bcrypt "admin-password") ; "$2a$10$jFdt4qKN1G2L2wv.HjkO.OR3y5ekcjovRHCS2ZVeOFYYv6RsgJ8Ky"

(app {:uri "/login?username=jane&password=user-password"}) ; {:status 200, :headers {"Content-Type" "text/html"}, :body "<pre>-------------------------------\nwhat the handler sees :\n Incoming Request:\n{:uri \"/login?username=jane&password=user-password\",\n :params {},\n :form-params {},\n :query-params {},\n :cemerick.friend/auth-config\n {:default-landing-uri \"/\",\n  :login-uri \"/login\",\n  :credential-fn #function[clojure.core/partial/fn--4759],\n  :workflows\n  [#function[cemerick.friend.workflows/interactive-form/fn--12166]]}}\n</pre><h1>Page Two</h1><pre>what the handler sees :\n Outgoing Response Map:\n{:status 200,\n :headers {\"Content-Type\" \"text/html\"},\n :body \"&lt;h1&gt;Page Two&lt;/h1&gt;\"}\n-------------------------------\n</pre>"}
(app {:uri "/page1?username=jane&password=user-password"}) ; {:status 200, :headers {"Content-Type" "text/html"}, :body "<pre>-------------------------------\nwhat the handler sees :\n Incoming Request:\n{:uri \"/page1?username=jane&password=user-password\",\n :params {},\n :form-params {},\n :query-params {},\n :cemerick.friend/auth-config\n {:default-landing-uri \"/\",\n  :login-uri \"/login\",\n  :credential-fn #function[clojure.core/partial/fn--4759],\n  :workflows\n  [#function[cemerick.friend.workflows/interactive-form/fn--12166]]}}\n</pre><h1>Page Two</h1><pre>what the handler sees :\n Outgoing Response Map:\n{:status 200,\n :headers {\"Content-Type\" \"text/html\"},\n :body \"&lt;h1&gt;Page Two&lt;/h1&gt;\"}\n-------------------------------\n</pre>"}
