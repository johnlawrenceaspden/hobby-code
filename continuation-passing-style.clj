;;Suppose we have an inc function in CPS

(defn cinc [n cont]
  (cont (+ n 1)))

;;How do we call it?

(cinc 3 print)

;;How do we call it twice?

(cinc 3 (fn[x] (cinc x print)))

;;What about if we'd like the value to go the the repl?

(cinc 3 (fn[x] (cinc x identity)))

;;How do we write a factorial?

;;Here's the usual tail-recursive version

(defn tfactorial [ n acc ]
  (if (= n 1) acc
      (tfactorial (dec n) (* n acc))))

(tfactorial 10 1)

;;That's easy to translate into CPS

(defn cfactorial [n acc cont]
  (if (= n 1) (cont acc)
      (cfactorial (dec n) (* acc n) cont)))

(cfactorial 1 1 identity)

(cfactorial 10 1 identity)

(cfactorial 1000 1 identity)

;;However (cfactorial 10000 1 identity) blows the stack.

;;Can we do it without building up stack?

;;Let's make it so that, if we want to call a function,  we instead return the function which we want to call next
(defn tfactorial [n acc cont]
  (if (= n 1) (cont acc)
      #(tfactorial (dec n) (* acc n) cont)))


;;Now
 
(tfactorial 1 1 identity)

;;just returns one

;;But 

(tfactorial 2 1 identity)

;;returns the function that tfactorial would like to call as its last meaningful act on this plane of existence
;;let us execute its last wishes for it.

((tfactorial 2 1 identity))

;;and by analogy
((((((((((tfactorial 10 1 identity))))))))))

;;Obviously there is a limit to the number of brackets that a man can type, so:

(defn executor [f & args]
  (let [lastwill (apply f args)]
    (if (fn? lastwill) (executor lastwill)
        lastwill)))

;;The executor will execute the function's last wish, and if that leads to a further wish, it will execute that, 
;;and so on ad inf...

(executor tfactorial 10 1 identity)

(executor tfactorial 1000 1 identity)

;;(executor tfactorial 10000 1 identity)
;;still blows the stack, but it is executor calling itself repeatedly now.

(defn tirelessexecutor [f & args]
  (let [lastwill (apply f args)]
    (if (fn? lastwill) (recur lastwill '())
        lastwill)))

;; ta tah......
(tirelessexecutor tfactorial 10000 1 identity)


;;Now, of course, we had to use recur to allow tireless executor to call itself. And we might as well just have
;;done that without all this cps and repeated call malarkey


(defn sanefactorial [ n acc ]
  (if (= n 1) acc
      (recur (dec n) (* n acc))))

(sanefactorial 10000 1)

;;And indeed, there's a bit of a performance hit with the return-a-function-to-be-called idiom.
(time (sanefactorial 10000 1))
(time (tirelessexecutor tfactorial 10000 1 identity))

;;But the CPS/executor idea allows us to solve a similar problem which recur can't help with

(declare r-odd? r-even?) ;;forward declarations in this day and age. I ask you....

(defn r-odd? [n]
     (if (= n 0) true
         (r-even? (dec n))))

(defn r-even? [n]
  (if (= n 0) false
      (r-odd? (dec n))))

(map r-odd? (range 20))

;;(r-odd? 10000) gives us the usual trouble with the stack breaking.

;;Let's rewrite them in continuation-passing-style:
(declare c-odd? c-even?)

(defn c-odd? [n cont]
     (if (= n 0) (cont false)
         (c-even? (dec n) cont)))

(defn c-even? [n cont]
     (if (= n 0) (cont true)
         (c-odd? (dec n) cont)))

(map #(c-odd? % identity) (range 20))

;;(c-odd? 10000 identity) boom!

;;Now let's just make them return the function they'd like to call, so that they don't hog stack space.

(declare ct-odd? ct-even?)

(defn ct-odd? [n cont]
  (if (= n 0) (cont true)
      #(ct-even? (dec n) cont)))

(defn ct-even? [n cont]
  (if (= n 0) (cont false)
      #(ct-odd? (dec n) cont)))


;;And now we can do a mutual tail-recursion without using stack:
(tirelessexecutor ct-odd? 10001 identity)

;;Which we can't do with recur.
;;Recur does, however, give us the ability to express an infinite loop, which is all we need for this trick.

;;Notice that the rewriting from tail-calling mutual recursion is absolutely trivial, despite involving the terrifying continuation-passing style, and will work for any case of mutual tail-calls. 

;;The trick is called trampolining, and it is in clojure's standard library.

;;I was looking for a tutorial on how to use it, and I couldn't find one, so I wrote this.

;;As I was trying to puzzle it out, I found that thinking of a tail-call as a function's last wish helped me to work out what was going on, whereas the rather more joyful image of a trampoline that the function is bouncing on was just confusing me.

;;Let's look at the clojure standard version:

(use 'clojure.contrib.repl-utils)
(source trampoline)

(comment
(defn trampoline
  "trampoline can be used to convert algorithms requiring mutual
  recursion without stack consumption. Calls f with supplied args, if
  any. If f returns a fn, calls that fn with no arguments, and
  continues to repeat, until the return value is not a fn, then
  returns that non-fn value. Note that if you want to return a fn as a
  final value, you must wrap it in some data structure and unpack it
  after trampoline returns."
  ([f]
     (let [ret (f)]
       (if (fn? ret)
         (recur ret)
         ret)))
  ([f & args]
     (trampoline #(apply f args))))
)

;;The difference between this and the version I came up with is rather subtle. I think I prefer mine.
;;You use it in exactly the same way:
(trampoline ct-odd? 10001 identity)
(trampoline ct-odd? 10000 identity)

;;Mine's slightly slower though, which explains why the library function is written the way it is.
(time (r-odd? 1001))
(time (tirelessexecutor ct-odd? 1001 identity))
(time (trampoline ct-odd? 1001 identity))

;;Presumably not having to call apply every time accounts for the difference.

;;Interestingly, although the trampoline is initially about the same speed as the tail recursive version, repeated executions of the tail recursion show a speedup of about a factor of two on my machine. Presumably the JVM is doing some optimization of the tail-calls which it can't do with the repeated calls.

;;Still, it's pretty fast, and if you're doing enough trampolining to worry about optimising it, you'll probably be close to blowing the stack anyway!




;;Finally, lets look at a real example of a mutual recursion:

;;We can work out the sine and cosine of numbers by saying that, for large x

;; cos 2x = (cos x * cos x) - (sin x * sin x)
;; sin 2x = 2 sin x cos x

;;Which allows us to work out the answer if we know the answer for x/2

;;and for small x, we can say that 

;;sin x = x
;;cos x = 1

;;The thing is, this should also be true for the imaginary numbers.

(declare sine cosine)

(defn add [[r1 i1] [r2 i2]]
  [(+ r1 r2) (+ i1 i2)])

(defn mul [[r1 i1] [r2 i2]]
  [(- (* r1 r2) (* i1 i2)) (+ (* r1 i2)(* r2 i1))])

(defn minus [[r i]]
  [(- r) (- i)])

(defn half [[r i]]
  [(/ r 2) (/ i 2)])

(defn abs [[r i]]
  (+ (* r r) (* i i)))

(defn sine [[r i]]
     (if (< (abs [r i]) 0.000001) [r i]
         (mul [2 0] 
              (mul 
               (sine   (half [r i])) 
               (cosine (half [r i])) ))))

(defn square [[r i]] 
  (mul [r i] [r i]))

(defn cosine [[r i]]
     (if (< (abs [r i]) 0.000001) [1 0]
         (add (square (cosine (half [r i]))) (minus (square (sine (half [r i])))))))

;;Here are some tests

(cosine [(/ 3.14159 2) 0]) ;(should be 0)
(sine [(/ 3.14159 2) 0])   ;(should be 1)

(cosine [0 (/ 3.14159 2)])  ;(should be 2.509)
(sine [0 (/ 3.14159 2)])    ;(should be 2.301)








































































































