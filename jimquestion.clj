What happens when we define a function?


First, let's start up a fresh clojure

user=> foo
java.lang.Exception: Unable to resolve symbol: foo in this context (NO_SOURCE_FILE:0)
user=> 


(defn foo "adds things" {:author "me", :version 4} [this that] (str this that))

After the definition is evaluated, 








(defn resolve1 [f]
        (let [t f]
          (resolve t)))

(resolve1 'foo)        -> #'user/foo
@(resolve1 'foo)       -> #<user$foo__2751 user$foo__2751@15b6aad>
;(resolve1 foo)

(defn resolve2 [f]
  (let [t 'f]
    (resolve t)))

(resolve2 'foo)        -> nil

(defmacro resolve3 [f]
  `(resolve '~f))

(resolve3 foo) -> #user/foo
