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

;; Some infrastructure


(defn html-escape [string] 
  (str "<pre>" (clojure.string/escape string {\< "&lt;", \> "&gt;"}) "</pre>"))

(defn wrap-spy [handler spyname include-body]
  (fn [request]
    (let [request (dissoc request :headers :ssl-client-cert :protocol :remote-addr :server-port :content-length :content-type :character-encoding :body :scheme :server-name)
          incoming (with-out-str
                     ;;(println "-------------------------------")
                     (println spyname "")
                     (clojure.pprint/pprint request))]
      (println incoming)
      (let [response (handler request)]
        (let [outgoing (with-out-str 
                         (println spyname "")
                         (clojure.pprint/pprint (if include-body response
                                                  (assoc response :body "#<?>")))
                         ;;(println "-------------------------------")
                         )]
          (println outgoing)
          (update-in response  [:body] (fn[x] (str (html-escape incoming) "\n\n" x "\n\n" (html-escape outgoing)))))))))

(declare handler)
(declare app)
(declare users)

(def app
  (-> #'handler
      (wrap-spy "what the handler sees" true)
      (ring.middleware.stacktrace/wrap-stacktrace)
      (ring.middleware.keyword-params/wrap-keyword-params)
      (ring.middleware.nested-params/wrap-nested-params)
      (ring.middleware.params/wrap-params)
      (wrap-spy "what the web server sees" false)
      ))


(defonce server (ring.adapter.jetty/run-jetty #'app {:port 8080 :join? false}))


;; Here's the actual app




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

(defn not-found [request]
    {:status 404
     :headers {"Content-Type""text/html"}
     :body (str  "<h1>404 ERROR</h1>")})


(defn handler [request]
  (case (request :uri)
    "/page1" (page1 request)
    "/page2" (page2 request)
    "/login" (login request)
    (not-found request)))



;; requests can be made programmatically, of course
(app {}) ; {:status 404, :headers {"Content-Type" "text/html"}, :body "<pre>what the web server sees \n{}\n</pre>\n\n<pre>what the handler sees \n{:params {}, :form-params {}, :query-params {}}\n</pre>\n\n<h1>404 ERROR</h1>\n\n<pre>what the handler sees \n{:status 404,\n :headers {\"Content-Type\" \"text/html\"},\n :body \"&lt;h1&gt;404 ERROR&lt;/h1&gt;\"}\n</pre>\n\n<pre>what the web server sees \n{:status 404, :headers {\"Content-Type\" \"text/html\"}, :body \"#&lt;?&gt;\"}\n</pre>"}

(defn summary [response]
  [(:status response) (re-seq #"<h1>.*</h1>" (:body response))])
                                        ;
(summary (app {})) ; [404 ("<h1>404 ERROR</h1>")]
(summary (app {:uri "/"})) ; [404 ("<h1>404 ERROR</h1>")]
(summary (app {:uri "/page1"})) ; [200 ("<h1>Page One</h1>")]
(summary (app {:uri "/page2"})) ; [200 ("<h1>Page Two</h1>")]
(summary (app {:uri "/page3"})) ; [404 ("<h1>404 ERROR</h1>")]
(summary (app {:uri "/login"})) ; [200 ("<h1>Login</h1>")]

;; Although for best effect one should either point a web browser at localhost:8080 or
;; watch -d -n 1 curl -s http://localhost:8080/

;; Now let's add friend

(require '[cemerick.friend :as friend])
(require '[cemerick.friend.credentials :as creds])
(require '[cemerick.friend.workflows :as workflows])


;; A quick check that we can access pages one and two
(summary (app {:uri "/page1"})) ; [200 ("<h1>Page One</h1>")]
(summary (app {:uri "/page2"})) ; [200 ("<h1>Page Two</h1>")]

(defn handler [request]
  (case (request :uri)
    "/page1" (friend/authorize #{::user} (page1 request))
    "/page2" (page2 request)
    "/login" (login request)
    (not-found request)))

(summary (app {:uri "/page1"})) ; [500 ("<h1>clojure.lang.ExceptionInfo</h1>")]
(summary (app {:uri "/page2"})) ; [200 ("<h1>Page Two</h1>")]


;; So it looks as though friend throws an exception inside the handler if you try to access
;; a page which is protected.

;; I'd like to examine that mechanism more closely, so let's find out how to call the handler directly

(def save-request (atom nil))

save-request ; #atom[nil 0x7c2068f5]

(defn handler [request]
  (swap! save-request (fn [_] request))
  (case (request :uri)
    "/page1" (friend/authorize #{::user} (page1 request))
    "/page2" (page2 request)
    "/login" (login request)
    (not-found request)))


save-request ; #atom[nil 0x7c2068f5]

(app {:uri "/page1"}) ; {:status 500, :headers {"Content-Type" "text/html"}, :body "<pre>what the web server sees \n{:uri \"/page1\"}\n</pre>\n\n<!DOCTYPE html>\n<html><head><title>Ring: Stacktrace</title><style type=\"text/css\">/*\nCopyright (c) 2008, Yahoo! Inc. All rights reserved.\nCode licensed under the BSD License:\nhttp://developer.yahoo.net/yui/license.txt\nversion: 2.6.0\n*/\nhtml{color:#000;background:#FFF;}body,div,dl,dt,dd,ul,ol,li,h1,h2,h3,h4,h5,h6,pre,code,form,fieldset,legend,input,textarea,p,blockquote,th,td{margin:0;padding:0;}table{border-collapse:collapse;border-spacing:0;}fieldset,img{border:0;}address,caption,cite,code,dfn,em,strong,th,var{font-style:normal;font-weight:normal;}li{list-style:none;}caption,th{text-align:left;}h1,h2,h3,h4,h5,h6{font-size:100%;font-weight:normal;}q:before,q:after{content:'';}abbr,acronym{border:0;font-variant:normal;}sup{vertical-align:text-top;}sub{vertical-align:text-bottom;}input,textarea,select{font-family:inherit;font-size:inherit;font-weight:inherit;}input,textarea,select{*font-size:100%;}legend{color:#000;}del,ins{text-decoration:none;}\n\nbody {\n    font-family: sans-serif;\n    background: #a00;\n    padding: 1em;\n}\n\n#exception {\n    background: #f2f2f2;\n    color: #333;\n    padding: 1em;\n}\n\nh1 {\n    color: #800;\n    font-size: 32pt;\n    text-align: center;\n    margin-bottom: 0.3em;\n}\n\n.message {\n    font-size: 16pt;\n    text-align: center;\n    margin-bottom: 1em;\n}\n\n#causes h2 {\n    font-size: 22pt;\n    text-align: center;\n    margin-bottom: 0.3em;\n}\n\n#causes h2 .class {\n    color: #800;\n}\n\n#causes .message {\n    font-size: 14pt;\n}\n\n.trace {\n    width: 90%;\n    margin: auto;\n}\n\n.trace table {\n    width: 100%;\n    font-size: 12pt;\n    background: #dadada;\n    border: 0.8em solid #dadada;\n    margin-bottom: 1.5em;\n}\n\n.trace table tr.clojure {\n    color: #222;\n}\n\n.trace table tr.java {\n    color: #6a6a6a;\n}\n\n.trace td {\n    padding-top: 0.4em;\n    padding-bottom: 0.4em;\n}\n\n.trace td.method {\n    padding-left: 1em;\n    padding-right: 0.2em;\n    text-aligh: left;\n}\n\n.trace td.source {\n    padding-left: 0.2em;\n    text-align: right;\n}\n\n.trace .views {\n    width: 100%;\n    background: #bcbcbc;\n    padding: 0.5em 0;\n}\n\n.views .label, .views ul, .views li {\n    display: inline-block;\n}\n\n.trace .views .label {\n    padding: 0 1em;\n}\n\n.trace .views li {\n    padding: 0 2em;\n    cursor: pointer;\n}\n</style></head><body><div id=\"exception\"><h1>clojure.lang.ExceptionInfo</h1><div class=\"message\">throw+: {:cemerick.friend/type :unauthorized, :cemerick.friend/identity nil, :cemerick.friend/required-roles #{:user/user}, :cemerick.friend/exprs [(page1 request)]}</div><div class=\"trace\"><table><tbody><tr class=\"clojure\"><td class=\"source\">support.clj:199</td><td class=\"method\">slingshot.support/stack-trace</td></tr><tr class=\"clojure\"><td class=\"source\">friend.clj:282</td><td class=\"method\">cemerick.friend/throw-unauthorized</td></tr><tr class=\"clojure\"><td class=\"source\">friend.clj:267</td><td class=\"method\">cemerick.friend/throw-unauthorized</td></tr><tr class=\"clojure\"><td class=\"source\">form-init5539524943909438024.clj:142</td><td class=\"method\">user/handler</td></tr><tr class=\"clojure\"><td class=\"source\">form-init5539524943909438024.clj:139</td><td class=\"method\">user/handler</td></tr><tr class=\"java\"><td class=\"source\">Var.java:379</td><td class=\"method\">clojure.lang.Var.invoke</td></tr><tr class=\"clojure\"><td class=\"source\">form-init5539524943909438024.clj:37</td><td class=\"method\">user/wrap-spy[fn]</td></tr><tr class=\"clojure\"><td class=\"source\">stacktrace.clj:23</td><td class=\"method\">ring.middleware.stacktrace/wrap-stacktrace-log[fn]</td></tr><tr class=\"clojure\"><td class=\"source\">stacktrace.clj:86</td><td class=\"method\">ring.middleware.stacktrace/wrap-stacktrace-web[fn]</td></tr><tr class=\"clojure\"><td class=\"source\">params.clj:64</td><td class=\"method\">ring.middleware.params/wrap-params[fn]</td></tr><tr class=\"clojure\"><td class=\"source\">keyword_params.clj:35</td><td class=\"method\">ring.middleware.keyword-params/wrap-keyword-params[fn]</td></tr><tr class=\"clojure\"><td class=\"source\">nested_params.clj:86</td><td class=\"method\">ring.middleware.nested-params/wrap-nested-params[fn]</td></tr><tr class=\"clojure\"><td class=\"source\">form-init5539524943909438024.clj:37</td><td class=\"method\">user/wrap-spy[fn]</td></tr><tr class=\"clojure\"><td class=\"source\">form-init5539524943909438024.clj:155</td><td class=\"method\">user/eval14280</td></tr><tr class=\"clojure\"><td class=\"source\">form-init5539524943909438024.clj:155</td><td class=\"method\">user/eval14280</td></tr><tr class=\"java\"><td class=\"source\">Compiler.java:6927</td><td class=\"method\">clojure.lang.Compiler.eval</td></tr><tr class=\"java\"><td class=\"source\">Compiler.java:6890</td><td class=\"method\">clojure.lang.Compiler.eval</td></tr><tr class=\"clojure\"><td class=\"source\">core.clj:3105</td><td class=\"method\">clojure.core/eval</td></tr><tr class=\"clojure\"><td class=\"source\">core.clj:3101</td><td class=\"method\">clojure.core/eval</td></tr><tr class=\"clojure\"><td class=\"source\">main.clj:240</td><td class=\"method\">clojure.main/repl[fn]</td></tr><tr class=\"clojure\"><td class=\"source\">main.clj:240</td><td class=\"method\">clojure.main/repl[fn]</td></tr><tr class=\"clojure\"><td class=\"source\">main.clj:258</td><td class=\"method\">clojure.main/repl[fn]</td></tr><tr class=\"clojure\"><td class=\"source\">main.clj:258</td><td class=\"method\">clojure.main/repl</td></tr><tr class=\"clojure\"><td class=\"source\">main.clj:174</td><td class=\"method\">clojure.main/repl</td></tr><tr class=\"java\"><td class=\"source\">RestFn.java:1523</td><td class=\"method\">clojure.lang.RestFn.invoke</td></tr><tr class=\"clojure\"><td class=\"source\">interruptible_eval.clj:87</td><td class=\"method\">clojure.tools.nrepl.middleware.interruptible-eval/evaluate[fn]</td></tr><tr class=\"java\"><td class=\"source\">AFn.java:152</td><td class=\"method\">clojure.lang.AFn.applyToHelper</td></tr><tr class=\"java\"><td class=\"source\">AFn.java:144</td><td class=\"method\">clojure.lang.AFn.applyTo</td></tr><tr class=\"clojure\"><td class=\"source\">core.clj:646</td><td class=\"method\">clojure.core/apply</td></tr><tr class=\"clojure\"><td class=\"source\">core.clj:1881</td><td class=\"method\">clojure.core/with-bindings*</td></tr><tr class=\"clojure\"><td class=\"source\">core.clj:1881</td><td class=\"method\">clojure.core/with-bindings*</td></tr><tr class=\"java\"><td class=\"source\">RestFn.java:425</td><td class=\"method\">clojure.lang.RestFn.invoke</td></tr><tr class=\"clojure\"><td class=\"source\">interruptible_eval.clj:85</td><td class=\"method\">clojure.tools.nrepl.middleware.interruptible-eval/evaluate</td></tr><tr class=\"clojure\"><td class=\"source\">interruptible_eval.clj:55</td><td class=\"method\">clojure.tools.nrepl.middleware.interruptible-eval/evaluate</td></tr><tr class=\"clojure\"><td class=\"source\">interruptible_eval.clj:222</td><td class=\"method\">clojure.tools.nrepl.middleware.interruptible-eval/interruptible-eval[fn]</td></tr><tr class=\"clojure\"><td class=\"source\">interruptible_eval.clj:190</td><td class=\"method\">clojure.tools.nrepl.middleware.interruptible-eval/run-next[fn]</td></tr><tr class=\"java\"><td class=\"source\">AFn.java:22</td><td class=\"method\">clojure.lang.AFn.run</td></tr><tr class=\"java\"><td class=\"source\">ThreadPoolExecutor.java:1145</td><td class=\"method\">java.util.concurrent.ThreadPoolExecutor.runWorker</td></tr><tr class=\"java\"><td class=\"source\">ThreadPoolExecutor.java:615</td><td class=\"method\">java.util.concurrent.ThreadPoolExecutor$Worker.run</td></tr><tr class=\"java\"><td class=\"source\">Thread.java:745</td><td class=\"method\">java.lang.Thread.run</td></tr></tbody></table></div></div></body></html>\n\n<pre>what the web server sees \n{:status 500, :headers {\"Content-Type\" \"text/html\"}, :body \"#&lt;?&gt;\"}\n</pre>"}

save-request ; #atom[{:uri "/page1", :params {}, :form-params {}, :query-params {}} 0x7c2068f5]

(handler {:uri "/page1", :params {}, :form-params {}, :query-params {}}) ; exception


;; this seems to be the important bit
(ex-data (try
    (handler {:uri "/page1", :params {}, :form-params {}, :query-params {}})
    (catch Exception e
      e)))

;; {:object
;;  {:cemerick.friend/type :unauthorized,
;;   :cemerick.friend/identity nil,
;;   :cemerick.friend/required-roles #{:user/user},
;;   :cemerick.friend/exprs [(page1 request)]},
;;  :environment
;;  {identity nil,
;;   authorization-info
;;   {:cemerick.friend/required-roles #{:user/user},
;;    :cemerick.friend/exprs [(page1 request)]}}}



(def app
  (-> #'handler
      (wrap-spy "what the handler sees" true)
      (ring.middleware.stacktrace/wrap-stacktrace)
      (friend/authenticate {:credential-fn (partial creds/bcrypt-credential-fn users)
                            :workflows [(workflows/interactive-form)]})
      (ring.middleware.params/wrap-params)
      (ring.middleware.keyword-params/wrap-keyword-params)
      (ring.middleware.nested-params/wrap-nested-params)

      (wrap-spy "what the web server sees" false)
      ))



(summary (app {})) ; [404 ("<h1>404 ERROR</h1>")]
(summary (app {:uri "/"})) ; [404 ("<h1>404 ERROR</h1>")]
(summary (app {:uri "/page1"})) ; [200 ("<h1>Page One</h1>")]
(summary (app {:uri "/page2"})) ; [200 ("<h1>Page Two</h1>")]
(summary (app {:uri "/page3"})) ; [404 ("<h1>404 ERROR</h1>")]
(summary (app {:uri "/login"})) ; [200 ("<h1>Login</h1>")]
(summary (app {:uri "/login" :request-method :post :params {"username" "jane", "password" "user-password"}}))


;; I have no idea how this fucking thing is supposed to work
;; perhaps this will help
https://adambard.com/blog/easy-auth-with-friend/


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Below here is messing about






;;      (friend/authenticate {:credential-fn (partial creds/bcrypt-credential-fn users)
;;                            :workflows [(workflows/interactive-form)]})















;; First we'll add a little user database 

(def users {"root" {:username "root"
                    :password (creds/hash-bcrypt "admin_password")
                    :roles #{::admin}}
            "jane" {:username "jane"
                    :password (creds/hash-bcrypt "user_password")
                    :roles #{::user}}})










;; hash-bcrypt is just a little hashing function, so that one doesn't store actual passwords anywhere
(creds/hash-bcrypt "some-stuff") ; "$2a$10$BIfIT5wrGWCrfDlYGdlrXuFlqP9c7l5yxDxApg2BSZZYjcrz0cxv2"


(app {:uri "/login"
      :query-string "username=jane&password=user-password"}) ; {:status 200, :headers {"Content-Type" "text/html"}, :body "<pre>-------------------------------\nwhat the handler sees :\n Incoming Request:\n{:uri \"/login?username=jane&password=user-password\",\n :params {},\n :form-params {},\n :query-params {},\n :cemerick.friend/auth-config\n {:default-landing-uri \"/\",\n  :login-uri \"/login\",\n  :credential-fn #function[clojure.core/partial/fn--4759],\n  :workflows\n  [#function[cemerick.friend.workflows/interactive-form/fn--12166]]}}\n</pre><h1>Page Two</h1><pre>what the handler sees :\n Outgoing Response Map:\n{:status 200,\n :headers {\"Content-Type\" \"text/html\"},\n :body \"&lt;h1&gt;Page Two&lt;/h1&gt;\"}\n-------------------------------\n</pre>"}

(app {:uri "/page1?username=jane&password=user-password"}) ; {:status 200, :headers {"Content-Type" "text/html"}, :body "<pre>-------------------------------\nwhat the handler sees :\n Incoming Request:\n{:uri \"/page1?username=jane&password=user-password\",\n :params {},\n :form-params {},\n :query-params {},\n :cemerick.friend/auth-config\n {:default-landing-uri \"/\",\n  :login-uri \"/login\",\n  :credential-fn #function[clojure.core/partial/fn--4759],\n  :workflows\n  [#function[cemerick.friend.workflows/interactive-form/fn--12166]]}}\n</pre><h1>Page Two</h1><pre>what the handler sees :\n Outgoing Response Map:\n{:status 200,\n :headers {\"Content-Type\" \"text/html\"},\n :body \"&lt;h1&gt;Page Two&lt;/h1&gt;\"}\n-------------------------------\n</pre>"}



