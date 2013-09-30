(clojure.core/-> 
 (java.io.File. "/tmp/form-init7935546848597144491.clj") 
 (.deleteOnExit))

(do (set! *warn-on-reflection* nil)
    (do nil 
        (try (clojure.core/require (quote clojure.tools.nrepl.server)) 
             (catch java.lang.Throwable t__6306__auto__ 
               (clojure.core/println "Error loading" (clojure.core/str (quote clojure.tools.nrepl.server) ":") 
                                     (clojure.core/or (.getMessage t__6306__auto__) (clojure.core/type t__6306__auto__)))))
        (try (clojure.core/require (quote complete.core))
             (catch java.lang.Throwable t__6306__auto__ (clojure.core/println "Error loading" (clojure.core/str (quote complete.core) ":") 
                                                                              (clojure.core/or (.getMessage t__6306__auto__) (clojure.core/type t__6306__auto__)))))
        [(println "hello from ~/.lein/profiles.clj")]
        ) 

    (clojure.core/let [server__6301__auto__ (clojure.tools.nrepl.server/start-server :bind "127.0.0.1" :port 4001 :ack-port 42615 :handler (clojure.tools.nrepl.server/default-handler))
                       port__6302__auto__ (clojure.core/-> server__6301__auto__ clojure.core/deref :ss .getLocalPort) 
                       repl-port-file__6303__auto__ (clojure.core/apply clojure.java.io/file ["/home/john/hobby-code" ".nrepl-port"])
                       legacy-repl-port__6304__auto__ (if 
                                                          (.exists (clojure.java.io/file "/home/john/hobby-code/target"))
                                                        (clojure.java.io/file "/home/john/hobby-code/target" "repl-port"))]
      (clojure.core/when false (clojure.core/println "nREPL server started on port" port__6302__auto__ "on host" "127.0.0.1"))
      (clojure.core/spit 
       (clojure.core/doto repl-port-file__6303__auto__ .deleteOnExit) 
       port__6302__auto__)
      (clojure.core/when legacy-repl-port__6304__auto__ 
        (clojure.core/spit (clojure.core/doto legacy-repl-port__6304__auto__ .deleteOnExit) port__6302__auto__))
      (clojure.core/deref (clojure.core/promise))))
