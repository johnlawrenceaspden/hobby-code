;; A couple of chaps have asked me to write a clojure macro tutorial,
;; to explain my debugging macro

;; I'm going to try to imagine that I didn't know how to write dbg,
;; and had to go at it by trial and error, to show why it is as it is.

;; The problem

;; Often the best way to debug code is by adding print statements.

;; Consider our old friend factorial

(defn factorial [n]
  (if (< n 2) n
      (* n (factorial (dec n)))))

(factorial 5) ;; 120

;; How would we watch it at work?

(defn factorial [n]
  (if (< n 2) n
      (* n (let [a (factorial (dec n))]
             (println "(factorial (dec n))=" a)
             a))))

(factorial 5)
;;(factorial (dec n))= 1
;;(factorial (dec n))= 2
;;(factorial (dec n))= 6
;;(factorial (dec n))= 24
;;120

;; So now we can watch the stack unwind. This gives us confidence in the inner
;; workings of the function.

;; The problem is that I've had to do a fair bit of typing to change the function
;; into a version that prints out its intermediate values.

;; Here's the original function again (re-evaluate the definition)
(defn factorial [n]
  (if (< n 2) n
      (* n (factorial (dec n)))))

(def n 5) ;; and let's give n a value so that fragments will run

;; Specifically, what I had to do was change
(factorial (dec n))
;; into
(let [a (factorial (dec n))] (println "(factorial (dec n))=" a) a)
;; Which is an expression which not only evaluates to 5, but prints out
;; (factorial (dec n))=5
;; at the same time










