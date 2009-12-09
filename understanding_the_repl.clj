;; Understanding the REPL

;; A REPL is a subtle thing.

;;Consider what happens when you type in 23

;;user>23
;;23

;;The program reads in the sequence of characters 2 3 <return>, and prints the sequence of characters \2 \3 <return>

;;It looks very much as if nothing has happened at all!

;;But in fact, a great deal has happened.

;;A REPL is a read-eval-print loop.

;;Any interaction with it involves reading in a stream of characters, which are passed to the function read.
;;The output from read is passed to the evaluator, eval
;;The output from eval is passed to the print function, and turned into a sequence of characters, which are then rendered on the screen by a mysterious process known as the operating system.

;;We can forget about the process which reads the keyboard and interprets physical movements as a stream of characters
;;We can also forget about the process which prints the characters on the screen.

;;Clojure would work in just the same way if it lived on a computer half-a-mile away, 
;;and you sent and recieved characters to it by Morse code.

;;So let us concentrate only on the three processes which take the character stream coming in and return the character stream going out.

;;Read, Eval, and Print.

;;One way to understand what is going on is to write your own read, eval, and print functions.

;;One way to get started on that project is to borrow the read and print functions from an existing lisp, and then use that lisp to write your own eval function, which is both the most interesting bit of the system, and the easiest to construct.

;;This is a traditional rite of passage for lispers, and I refer you to chapter 4 of the Structure and Interpretation of Computer Programs if you wish to pursue it. 

;;Now most lisps will give us the power to call their read, eval, and print functions, and very useful they are too.

;;But Clojure is a lisp written in java, and it also gives us the power to create objects in the underlying java system.

;;And we can use this power to conduct experiments on the three functions.

;;Let's look a little at the reader first.

;;The reader reads characters from any stream which is of type java.io.PushbackReader

;;We can create one of these streams from our REPL!

;;Remember that clojure is just a java program. We could have created these streams in a java program, but we will use clojure's REPL to manipulate them. We must remember that whenever anything is typed in, it is the reader and the evaluator that interpret them, and the printer that displays the result, but if we are careful we can pick the process apart.

;;So here's our input stream with the characters 2,3,<return> in it.

(java.io.PushbackReader. (java.io.StringReader. "23\n"))
#<PushbackReader java.io.PushbackReader@1f9f0f2>

;;Remember, we typed in (...). The reader interpreted that stream of characters, and passed it to the evaluator.

;;The evaluator, as a result created a java.lang.String, wrapped that in a java.io.StringReader, wrapped that in a java.io.PushbackReader, and then gave that object to the printer, which decided that an appropriate printed representation would be the sequence of characters #,<,P,u,s,...........2,>,<return>

;;Let's instead assign it to a value

(def inputstream (java.io.PushbackReader. (java.io.StringReader. "23\n")))
#'user/inputstream

;;Now the evaluator has done much the same thing, but then rather than handing the PushbackReader to the print function, it has attached it to the var #'user/inputstream, noted that the symbol foo should be associated in the user namespace with the var #'user/inputstream, and then passed the var to the printer, which has decided that #,',u,s,....,m is an appropriate sequence of characters with which to represent the var to the user.

;;We can check that the symbol->var mapping is now in the user namespace:

(ns-publics 'user)
{inputstream #'user/inputstream}

;;This is a side-effect of evaluating a def form. In fact the return value of the expression is not particularly interesting. It is the side-effect that we are after.

;;Since we've only just started our REPL, the user namespace is empty apart from the association that the evaluator has just made.

;;Now the question in which we are interested is: What does the reader produce when it is given a PushbackReader which yields the characters 2,3,<return> ?

;;We can ask the REPL
(read inputstream)
23

;;And it tells us that it is a thing which the printer chooses to represent as 2,3,<return>

;;PushbackReaders are mutable objects, and this one is now exhausted. It has no more characters to give.

;;Let us create another similar stream, pass it again to the reader, and this time save the value that the reader gives us.

(def read23 (read (java.io.PushbackReader. (java.io.StringReader. "23\n"))))
#'user/read23

;;Another var has been created by the def, and the printer has turned it into a stream of characters for us.

;;But we can examine the object to which read23 refers:

(type read23)
java.lang.Integer

;;Aha! the reader returned, not the string of characters 2,3,<return>, but an object of type java.lang.Integer
;;I wonder what else we can learn about it?

;;Remember that it is a java class, and that we can use clojure to call java methods on it.

(.getMethods (class read23))
#<Method[] [Ljava.lang.reflect.Method;@1aac775>

(map str (.getMethods (class read23)))
;;java.lang.Integer is a class with many methods
;;and we will have an easier time understanding it if we cheat.

(use 'clojure.contrib.repl-utils)
(javadoc read23)

;;let's try a method. doubleValue returns the value as a string.
(.doubleValue read23)
23.0

;;Unless the printer is being incredibly perverse, we are dealing with a java.lang.Integer whose value is 23.

;;But notice that this is a very different thing from a character stream made from 2,3,<return>

;;Let's forget all about the details of creating streams, and instead use the read-string function,
;;which can be fed strings directly.

;;What does the reader return when we give it the characters (,+,<space>,2,<space>,2,)<return>?

(def twoplustwo (read-string "(+ 2 2)\n"))
#'user/twoplustwo

(type twoplustwo)
clojure.lang.PersistentList

;;That's a new one.

(.count twoplustwo)
3

;;A list with three things in it.

(map type twoplustwo)
(clojure.lang.Symbol java.lang.Integer java.lang.Integer)

;;A list of one symbol, and two integers.

(javadoc clojure.lang.Symbol)

(.getName (first twoplustwo))
"+"

(.doubleValue (first (rest twoplustwo)))
2.0

;;So the brackets have gone. The reader's interpretation of the string "(+ 2 2)" is a list of a clojure.lang.Symbol whose name is the (java.lang.)String "+", and two java.lang.Integers whose value as doubles is 2.0.

;;I reckon we have this reader thing pretty well sorted out.
;;But look at this:

(def line-noise (read-string "'foo"))
#'user/line-noise

(type line-noise)
clojure.lang.Cons

(.first line-noise)
quote

(type (.next line-noise))
clojure.lang.PersistentList

(.count (.next line-noise))
1

(type (.first (.next line-noise)))
clojure.lang.Symbol

(.getName (.first (.next line-noise)))
"foo"

;;So it appears that when the reader's presented with ',f,o,o
;;It produces a Cons, whose first element is the Symbol whose name is "quote", and whose next element is a list, whose sole element is the Symbol "foo".

;;The printer represents this little tree as (quote foo).

;;But if we wrote our own printer we might well call it (cons (symbol "quote") (list (symbol "foo")))

;;indeed this is a clojure expression:

(cons (symbol "quote") (list (symbol "foo")))

;;which we can type in at the REPL, which will return the same string as if we typed in (read-string "'foo")

;;It appears that there are some subtleties to the reader.






























