(use 'compojure)

(defn layout [title & body]
  (html
   [:head [:title title]]
   [:body [:h2 title] body]))

;(layout "******" "++++++")

(defn post-form snippet []
  (layout "Send a form"
   (form-to [:post "/"]
	    (text-area {:rows 20 :cols 73} "body")
	    [:br]
	    (submit-button "Save"))))

(defn receive-form [body]
  (layout "Form received"
    [:div [:pre body]]))

;; (defn show-snippet [id]
;;   (let [snippet (select-snippet id)]
;;     (layout (str "Snippet " id)
;;      [:div [:pre [:code.clojure (:body snippet)]]]
;;      [:div (:created_at snippet)])))

;(insert-snippet "for(i=0;i<10;i++){};")
;(show-snippet 13)

;; (defn create-snippet [body]
;;   (if-let [id (insert-snippet body)]
;;     (redirect-to (str "/" id))
;;     (redirect-to "/")))

;(create-snippet "this is too easy")
;(show-snippet 8)


(defroutes snippet-app
  "main routing function"

;  (GET "/public/*"
;    (or (serve-file "/home/john/clojure-programs/programming-clojure/public" (params :*)) :next))
;  (GET "/:id" (show-snippet (params :id)))
;  (POST "/" (create-snippet (:body params)))
  (GET "/" (post-form))
  (POST "/" (receive-form (:body params)))
  (GET "/:id" (layout "Request" (str  "yo!:this is the page for:" (params :id))))
  (ANY "*"
       (page-not-found "/home/john/clojure-programs/programming-clojure/public/404.html")))


;;Run the server. If we reload this file into a running image then run-server
;;throws an exception. 
(import '(java.net BindException))

(let [port 8080]
  (try (run-server {:port port} "/*" (servlet snippet-app))
     (println (str "Server started on port " port "."))
     (catch java.net.BindException _
	 (println (str "Failed to start server on port " port ". Already running?")))))



