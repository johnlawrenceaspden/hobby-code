;; http://stackoverflow.com/questions/14686807/how-do-i-make-this-macro-variadic-in-clojure/14688118#14688118


(defmacro ds3 [a b c] 
     `(clojure.string/join ", " 
          [(str '~a "->" ~a) 
           (str '~b "->" ~b) 
           (str '~c "->" ~c)]))

(def ps print-str)

(defmacro ds
  ([a]  `(ps '~a "->" ~a))
  ([a b]  `(clojure.string/join ", "  [(ps '~a "->" ~a) (ps '~b "->" ~b)]))
  ([a b c]  `(clojure.string/join ", "  [(ps '~a "->" ~a) (ps '~b "->" ~b)  (ps '~c "->" ~c)]))
  ([a b c d]  `(clojure.string/join ", "  [(ps '~a "->" ~a) (ps '~b "->" ~b)  (ps '~c "->" ~c) (ps '~d "->" ~d)]))
  ([a b c d e] `(ps (ds ~a ~b ~c ~d) ", " (ds ~e))))


;; The real answer:

(defmacro ds [& symbols]                                                                                                                             
  `(clojure.string/join ", "                                                                                                                         
                        ~(into [] 
                           (map (fn [s] `(str ~(name s) "->" ~s))  symbols))))


(let [ a 1 b 2 c 3 d 4 e 5]
  [(ds a)
   (ds a b)
   (ds a b c) (ds a b c d ) (ds a b c d e )])