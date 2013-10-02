;; Efficiency and Progress IV: Avoiding Leiningen

;; It turns out that there are many relevant variables when attempting
;; to speed up Clojure.

;; Java can start in 'client mode' or 'server mode'. The difference
;; seems to be that 'server mode' is faster, and speeds up as you run
;; it.  

;; It's also, rather amazingly, true that leiningen turns off the
;; default jvm runtime optimizations!

;; You're supposed to be able to control this by adding

  :jvm-opts ^:replace ["-server"]

;; to project.clj, and initially this did seem to work for me.

;; However now it's stopped working, and things that were fast have
;; become slow.

;; You can get the classpath that leiningen would use like this:

LEIN_CLASSPATH=`lein classpath`

;; And then start a repl with:

rlwrap java -server -classpath $LEIN_CLASSPATH clojure.main 

;; Apart from the initial run of leiningen to work out the classpath,
;; which you only have to do occasionally, this is a much quicker way
;; to start a repl, and rlwrap provides a command-line environment
;; that works like the bash shell and which I find very nice.

;; Of course, you'll want to make a version that will talk to emacs
;; via nrepl, and providing that the nrepl jar is on the classpath,
;; this will do the trick:

rlwrap java -server -classpath $LEIN_CLASSPATH clojure.main -e "( do (require 'clojure.tools.nrepl.server) (clojure.tools.nrepl.server/start-server :bind \"127.0.0.1\" :port 4001))" -r

;; In fact, if you have clojure-1.5.1 and nrepl 0.2.3 in your maven repository, then you can create a minimal classpath like this:
CLP=$HOME/.m2/repository/org/clojure/clojure/1.5.1/clojure-1.5.1.jar:$HOME/.m2/repository/org/clojure/tools.nrepl/0.2.3/tools.nrepl-0.2.3.jar
;; And then run the clojure/repl/nrepl process like this:
rlwrap java -server -classpath $CLP clojure.main -e "( do (require 'clojure.tools.nrepl.server) (clojure.tools.nrepl.server/start-server :bind \"127.0.0.1\" :port 4001))" -r

;; And there are many variations on this theme.



;; Recap

;; This is slow:
(time (reduce + (map + (range 1000000) (range 1000000))))
"Elapsed time: 2237.631681 msecs"
999999000000

;; C can perform roughly equivalent task in 8.6 ms, and Java in around 20 ms

;; So do we have to drop into C or Java when we want to make algorithms fast?
;; I hope not! 

;; It turns out that there are many relevant variables 
(set! *warn-on-reflection* true)
(set! *unchecked-math* true)
(clojure-version) ;-> "1.5.1"
((into{} (System/getProperties)) "java.vm.version") ;-> "23.7-b01"
((into{} (System/getProperties)) "java.vm.name") ;-> "OpenJDK Server VM"

;; Java can start in 'client mode' or 'server mode'. The difference
;; seems to be that 'server mode' is faster, and speeds up as you run
;; it.  

;; It's also, rather amazingly, true that leiningen turns off the
;; default jvm runtime optimizations!

;; Leiningen is a useful for stopping one from having to use XML for
;; dependencies, but I'd rather it not do this sort of thing, so I've
;; taken to starting repls like:

;; LEIN_CLASSPATH=`lein classpath`
;; rlwrap java -server -classpath $LEIN_CLASSPATH clojure.main 

;; Apart from the initial run of leiningen to work out the classpath,
;; this runs much faster, and rlwrap provides a much better
;; command-line environment that works like the bash shell.

;; Of course, you'll want to make a version that will talk to emacs via nrepl, and providing that the nrepl jar is on the classpath, this will do the trick:

;; rlwrap java -server -classpath $LEIN_CLASSPATH clojure.main -e "( do (require 'clojure.tools.nrepl.server) (clojure.tools.nrepl.server/start-server :bind \"127.0.0.1\" :port 4001))" -r















































;; Sadly the code is completely unreadable, but we may be able to do something about that. 
;; We are in a lisp after all:

;; It turns out there are macros for doing this sort of thing:

(def N 1000000)
(def a (int-array (range N)))
(def b (int-array N))

(time 
 (let [a ^ints a]
   (areduce a i ret 0 (+ ret (aget a i)))))

"Elapsed time: 46.748545 msecs"
499999500000

;; It's worth examining what areduce is doing
(macroexpand '(areduce a i ret 0 (+ ret (aget a i))))
;; (let* [a__4717__auto__ a] (clojure.core/loop [i 0 ret 0] (if (clojure.core/< i (clojure.core/alength a__4717__auto__)) (recur (clojure.core/unchecked-inc i) (+ ret (aget a i))) ret)))

(time (let [a ^ints a]
        (let* [a__4717__auto__ a] 
              (clojure.core/loop [i 0 ret 0] 
                (if (clojure.core/< i (clojure.core/alength a__4717__auto__)) 
                  (recur (clojure.core/unchecked-inc i) (+ ret (aget a i))) ret)))))
"Elapsed time: 46.674857 msecs"

;; edited for clarity
(time (let [a ^ints a] 
            (loop [i 0 ret 0] 
              (if (< i (alength a)) 
                (recur (unchecked-inc i) (+ ret (aget a i))) ret))))
"Elapsed time: 40.352545 msecs"
;; weirdly the editing seems to have actually speeded it up. I have absolutely no idea why.

;; And there's also amap, which seems weirdly slower, even though it's doing three array accesses instead of one.
(time (let [a ^ints a b ^ints b] 
        (amap a i ret (+ (aget b i)(aget a i)))))
"Elapsed time: 203.306294 msecs"
#<int[] [I@116fc35> ;; Apparently this means 'integer array'

;; It turns out that amap does what my iteration did but without mutation, by making a copy
(macroexpand '(amap a i ret (+ (aget b i)(aget a i))))

(time 
 (let [a ^ints a b ^ints b]
   (let [ret (clojure.core/aclone a)] 
     (loop [i 0] 
       (if (< i (alength a)) 
         (do (clojure.core/aset ret i (+ (aget b i) (aget a i))) 
             (recur (clojure.core/unchecked-inc i))) 
         ret)))))
"Elapsed time: 187.263691 msecs"
#<int[] [I@b3f01d> ;; Again, this seems to have got slightly faster just because I tidied it up.




;; Let's try the long calculation again using amap and areduce

(time
 (let [a ^ints a b ^ints b]
   (loop [count 0 sum 0 b b]
     (if (< count 1000)
       (do (println count sum)
           (let [b (amap a i ret (+ (aget b i) (aget a i)))]
             (let [sum (+ sum (areduce b i ret 0 (+ ret (aget b i))))]
               (recur (unchecked-inc count) sum b))))
       sum))))

;; This just freezes solid and I've no idea why. I can't actually understand it.



(time
 (let [a ^ints a b ^ints b]
   (loop [count 0 sum 0 b b]
     (if (= count 1000) sum
         (do (println count sum)
             (recur (unchecked-inc count) (+ sum (areduce b i ret 0 (+ ret (aget b i)))) (amap a i ret (+ (aget b i) (aget a i)))))))))





(def N 1000000)
(def a (int-array (range N)))
(def b (int-array N))


(time 
 (let [a ^ints a b ^ints b]
   (loop [count 0 sum 0]
     (if (= count 1000) sum
         (do 
           (println count sum)
           (dotimes [i N] (aset b i (+ (aget a i)(aget b i))))
           (recur (inc count) (+ sum (areduce b i ret 0 (+ ret (aget b i))))))))))

     
 "Elapsed time: 126538.750983 msecs"
250249749750000000
















   

(def N 1000000)
(def a (int-array (range N)))
(def b (int-array N))


(time 
 (let [a ^ints a b ^ints b]
   (loop [count 0 sum 0 b ^ints b]
     (if (= count 1000) sum
         (do 
           (println count sum)
           (let [b ^ints (amap b i ret (+ (aget a i) (aget b i)))]
             (recur (inc count) (+ sum (areduce b i ret 0 (+ ret (aget b i)))) b)))))))
    
form-init4591430370612349788.clj:8 recur arg for primitive local: sum is not matching primitive, had: Object, needed: long
Auto-boxing loop arg: sum

"Elapsed time: 59580.516979 msecs"
250249749750000000


 












;; There are actually helpful macro for doing such things
(time 
 (let [b ^ints b]
   (areduce b i ret 0 (+ ret (aget b i)))))
"Elapsed time: 100.587301 msecs"
999999000000

;; here's the map version. It's a bit difficult to understand what it
;; does unless you're used to constructing the loops by hand
(time 
 (let [b ^ints b a ^ints a]
   (amap b i ret (+ (aget a i) (aget b i)))))



(macroexpand '(amap b i ret (+ (aget a i) (aget b i))))
