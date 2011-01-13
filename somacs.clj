(defmacro test-macro
 [& args]
 `(println (str "count=" ~(count args) "; args=" ~@args)))

(defn test-fn-calling-macro
 [& args]
 (test-macro args))

(defn test-fn-expanding-macro-at-runtime
  [& args]
  (eval (cons `test-macro args)))

(defmacro test-macro-expanding-macro-at-compile-time
  [& args]
  (cons `test-macro args))

;; using the backquote and splicing notation

(defmacro test-macro-expanding-macro-at-compile-time-2
  [& args]
  `(test-macro ~@args))

(defn test-fn-expanding-macro-at-runtime-2
  [& args]
  (eval `(test-macro ~@args)))



(test-macro "a" "b" "c") ;; count=3; args=abc nil
(test-fn-calling-macro "a" "b" "c") ;; count=1; args=("a" "b" "c") nil

(test-fn-expanding-macro-at-runtime "a" "b" "c") ; count=3; args=abc nil
(test-macro-expanding-macro-at-compile-time "a" "b" "c") ; count=3; args=abc nil
(test-macro-expanding-macro-at-compile-time-2 "a" "b" "c") ; count=3; args=abc nil
(test-fn-expanding-macro-at-runtime "a" "b" "c") ; count=3; args=abc nil


