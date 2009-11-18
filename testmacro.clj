 ;;The other day while writing a program, 
;;I found myself repeatedly typing the same code over and over again.

;;To avoid unnecessary explanations, let us imagine that it was the factorial program.

(defn factorial [n]
  (if (= n 0) 1 (* n (factorial (dec n)))))

;;Because factorial is such a tricky program, and so likely to go wrong, I find myself paranoid testing the function

(factorial 1)
(factorial 10)
(map factorial (range 10))

;;I'm using an environment where with a single click I can evaluate a piece of code, so I write
;;these little paranoid checks into the file, and then whenever I modify the function I can go
;;and re-evaluate the tests to check that I haven't broken it.

;;eventually I find that, while I can't be bothered going back and doing this too often,
;;I'd like my little set of regression tests to be run every time I recompile the file.
;;This catches many mysterious bugs, and is generally worth its weight in gold. 
;;I find if this is going on, then I don't miss the security of static type checking.

;;It's also nice to have the original tests still in the file, so that I can execute them explicitly
;;if I want to, and they also serve as handy documentation for the function.

;;What I end up writing is

(and 
 (= (factorial 1) 1)
 (= (factorial 10) 3628800)
 (= (map factorial (range 10)) '(1 1 2 6 24 120 720 5040 40320 362880)))

;;emacs' handy evaluate-and-put-the result-into-the-file function (C-uxe) is invaluable here.

;;Here is a single expression which gets executed every time the file is loaded, 
;;which runs my factorial function in a number of cases.

;;That's actually really handy in itself, and I can execute it manually, 
;;and I can execute the sub-expressions.

;;But it silently throws the result away if the test fails on load, so although it will catch
;;type errors, it doesn't catch bugs.

;;So I change it to be:

(let [test
      (and 
       (= (factorial 1) 1)
       (= (factorial 10) 3628800)
       (= (map factorial (range 10)) '(1 1 2 6 24 120 720 5040 40320 362880)))]
  (println "factorial" "tests" (if test "pass" "fail"))
  test)

;;In fact the code that I usually use in scheme is more complicated than this, and will tell me
;;how many tests passed, and which tests failed, and what the answers for the failing tests were
;;and what they should have been, and it will deal with
;;exceptions that get thrown, and it will deliberately cause exceptions and check that they
;;happen as expected, and it will redirect standard output and standard error so that the anything
;;printed by the functions can be checked too, etc, etc, but this is a good start.

;;But at this point I'm already annoyed that after every function that I write, I have to repeat
;;a block of code like this, and I find myself using cut and paste a lot, which is never a good sign,
;;and if I want to improve the test code I find myself making the same changes in various 
;;different places, and in short my senses are screaming for an abstraction of some sort.

;;In fact for this example a functional approach would work, but I know from doing this many times
;;before in various languages that it always ends up being difficult when you come to reporting 
;;errors, and so you end up, if you can, doing it as a macro eventually, so I'll make it a macro
;;from the start.

;;Now, what I want is to be able to write

(testset "factorial"
         (= (factorial 1) 1)
         (= (factorial 10) 3628800)
         (= (map factorial (range 10)) '(1 1 2 6 24 120 720 5040 40320 362880)
)

;;and have the compiler treat it as if I had written:

(let [test
      (and 
       (= (factorial 1) 1)
       (= (factorial 10) 3628800)
       (= (map factorial (range 10)) '(1 1 2 6 24 120 720 5040 40320 362880)))]
  (println "factorial" "tests" (if test "pass" "fail"))
  test)

;;; or getting rid of the specifics:

(testset title test1 test2 ...) ->

(let [test (and
            test1 
            test2
            ...
            )
      ]       
  (println title "tests" (if test "pass" "fail"))
  test)

;; and this is a very easy macro to write.

(defmacro testset [title & tests]
  `(let [test# (and 
                ~@tests)
         ]       
     (println ~title "tests" (if test# "pass" "fail"))
     test#))

;; note how we use the backquote syntax so that the macro looks very like the desired code
;; and ~ (unquote) to put the title where it should be
;; and ~@ (splicing unquote) to put the variable argument list of tests into the code as 
;; if they'd been typed there one by one, and not as a list
;; and we've got a local variable test which we want to get generated as an opaque symbol so 
;; that it won't interfere with any other variables, so we use clojure's auto-gensym facility
;; to do this automatically, just by calling it test#

;; we can use macroexpand to see what's going on

(macroexpand '(testset "hi" (= (println "Hello World") nil)))

;;on my machine the generated code looks like:

(let* [test__6260__auto__ (clojure.core/and 
                           (= (println "Hello World") nil)
                           )] 
      (clojure.core/println "hi" "tests" (if test__6260__auto__ "pass" "fail")) 
      test__6260__auto__)

;;and we can see that clojure is doing something clever in its backquote syntax that
;;means that the println and the and in the macro are getting resolved into the namespace where the
;;macro was defined, but the if, and the println that was in the test are getting left unresolved.

;;And I have to confess that I am not sure what is going on here, but it seems to work!

;;It worries me that I don't understand the macro mechanism here.

;;In classic lisp, macros are very simple and powerful things, 
;;but there are a couple of traps into which it is easy
;;to fall for beginners. But they are not too difficult to avoid if you understand how things work

;;In classic scheme, there is a hygenic macro mechanism, which is hard to understand
;;but results in the traps not being there. But it also results in some of the power being
;;difficult to get at. Once you actually grok it, however, it's a dream to use.

;;Clojure appears to have taken a middle path which I do not yet understand.
;;Macros seem easy to write and the usual traps seem covered.

;;I worry that there may be different traps for the unwary.

;;But it is so useful that I am doomed to find them, if they are there....

;;And this is a good thing!

