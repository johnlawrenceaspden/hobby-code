(import '(java.io IOException)
	'(java.net Socket)
	'(java.net InetSocketAddress)
	'(java.net SocketTimeoutException)
	'(java.net UnknownHostException)
	'(java.net ConnectException))

(def *timeout* 1000)
(def max-open-ports 1000)
(def *verbose* 10)

(defn loglevel [level str]
  (when (>= *verbose* level) 
    (println str))
  str)

(defn log [str]      (loglevel 2 str))
(defn progress [str] (loglevel 0 str))

(defn port-open? [hostname port]
  (let [fail    (fn [excp] (log (str excp)))
	succeed (fn []     true)]
    (let [sock-addr (InetSocketAddress. hostname port)]
      (try 
       (log (str "opening:" hostname ":"  port))
       (with-open [sock (Socket.)]
	 (. sock connect sock-addr *timeout*) 
	 (succeed))
       (catch SocketTimeoutException e (fail e))
       (catch UnknownHostException e   (fail e))
       (catch ConnectException e       (fail e))))))

(defn test-and-update [connection]
  (let [ip (:ip connection) port (:port connection)]
    (let [result (port-open? ip port)]
      (if (= result true)
	(do (log (str ip port "success")) 
	    (assoc connection :open true :comment "success"))
	(do (log (str ip port "fail: " result))
	    (assoc connection :open false :comment result))))))

(defn gradual [n agents fn]
  "send fn to the agents, n at a time, waiting for each batch to finish"
  (progress (str "scans remaining" (count agents)))
  (let [nowagents (take n agents)
	delayedagents (drop n agents)]
    (doseq [agent nowagents] (send-off agent fn))
    (apply await nowagents)
    (if (not (empty? delayedagents))
      (recur n delayedagents fn))))


(defn netrange [prefix r] (for [x (range r)] (str prefix x)))
(defn localnetwork [r] (concat (netrange "192.168.0." r) (netrange "192.168.1." r)))
(defn localhost [] '("127.0.0.1"))
(def commonports '(22 80 631 8080))
(def lowports (concat (range 1024)))

(def iplist (concat (localhost) (localnetwork 1)))
(def portlist (concat commonports))

(defstruct connection :ip :port :open :comment)

(defn connection-agents [iplist portlist]
  (for [ip iplist port portlist] 
    (agent (struct connection ip port "unknown" "not tried"))))

(defn scan [iplist portlist fn]
  (let [agents (connection-agents iplist portlist)]   
    (gradual max-open-ports agents fn)
    (map deref agents)))

(defn open-ports [iplist portlist]
  (filter :open (scan iplist portlist test-and-update)))

(defn make-tu [verbosity]
  (fn [connection] (binding [*verbose* verbosity] (test-and-update connection))))

(defn open-ports-verbose [iplist portlist verbosity]
  (filter :open (scan iplist portlist (make-tu verbosity))))

;(map #(format "%s:%s\n" (:ip %) (:port %)) )

;;;sanity checks
(defn tst []
  (binding [*verbose* 10]
    (doall (map (fn [[ip port]] [ip port (port-open? ip port)])
		(list ["127.0.0.1" 8080] 
		      ["127.0.0.1" 8081]	     
		      ["192.168.1.1" 80]
		      ["192.168.1.1" 81]
		      ["www.google.com" 80])))))
(tst)
