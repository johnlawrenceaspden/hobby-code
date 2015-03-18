;; Introduction to the Lambda Calculus

;; More precisely, an introduction to the algorithmic language Scheme, which is what you get if you start with 
;; the lambda calculus and you trick it out with some extra stuff that often comes in handy, true and false and if 
;; and define and also some types of numbers, like integers and fractions, and adding, and multiplying.

;; You can build all that stuff starting from scratch with just lambda, and it's a nice thing to do if you want 
;; to understand how it all works, but I reckon you're already ok at that sort of thing. 

;; So we'll start from something that can do basic arithmetic, and we'll learn how to find square roots of things.

;; This is an evaluator. You can ask it the values of things.

2
3
+

;; We can apply the procedure to the two numbers

(+ 2 3)


;; Can you tell me the square of 333?

(* 333 333)

;; The brackets mean (work out the value of the things in the brackets, and then do the first thing to the other things)

;; So what do you get if you add the squares of 3 and 4?

(+ (* 3 3) (* 4 4))


;; We have procedures for * and + , but if we ask the evaluator what & means, or what square means
;; it will just say 'I have no clue'.

;; It might be nice if we had a procedure for squaring things

;; How you make a procedure is with this thing called lambda, which is sort of a rewriting sort of thing.

;; Try (lambda (x) (* x x)), which means 'make me a thing which, when I give the thing x, gives me the value of (* x x) instead' 

(lambda (x) (* x x))

;; #<procedure>, it says, which is very like what you get when you type in +, and it says #<procedure:+>.

;; So we hope we've made a procedure like + or *

;; How shall we use it to get the square of 333?

((lambda (x) (* x x)) 333)

;; Now obviously, typing out (lambda (x) (* x x)) every time you mean square is not brilliant, 
;; so we want to give our little squaring-thing a name.

(define square (lambda (x) (* x x)))

;; Now how do we find the square of 333?

(square 333) ; 110889

;; So lambda is allowing us to make new things, to turn complicated procedures into simple things 

;; and define is allowing us to give things names

;; So now let's make a procedure that takes two things, and squares them both, 
;; and adds the squares together, and let's call it pythag

(define pythag 
  (lambda (x y) 
    (+ (square x) (square y))))

(pythag 3 4)

;; OK, great, now can you figure out how the procedure < works?

( < 3 4)
( < 4 3)
( < 3 4 6)
( < 3 4 2)

;; Notice that these #t and #f things are things that the evaluator knows the value of:
;; They're called true and false.

#f
#t

;; So now the last piece of the puzzle, if. if takes three things:

(if #t 1 2) ;1
(if #f 1 2) ;2

;; So we've got numbers and *,+,-,/, and we've got #t #f and if, and we've got lambda, and define

;; And so all the stuff we've got above, we can think of it as a reference manual for a little language

;; We can build the whole world out of this little language. That's it. 
;; That's what God used to build the universe, and any other universes that might have come to His mind.
;; And we can use it too.

;; and here's our manual

2
*
(* 2 3)
(define forty-four 44)
forty-four
(lambda (x) (* x x))
((lambda (x) (* x x)) 3)
(if (< 2 3) 2 3)

;; And if we understand these few lines, then we understand the whole thing, and we can fit the little pieces together like this:

(define square (lambda (x) (* x x)))
(square 2)

;; So now I want you to use the bits to make me a function, call it absolute-value, which if you give it a number gives you back
;; the number, if it's positive, and minus the number, if it's negative.

(define absolute-value (lambda (x) (if (> x 0) x (- x))))

(absolute-value 1)
(absolute-value -3)
(absolute-value 0)

;; So I've taught you most of the rules for Scheme, which is a sort of super-advanced lambda calculus, and so if you understand 
;; the bits above, then you've got the hang of the lambda calculus plus some more stuff.

;; And it's a bit like chess. The rules of chess are super-simple, you can explain them to babies, 
;; like Dr Polgar did to Judit and her sisters.
;; But that doesn't make the babies into good chess players yet. They have to practise.

;; How are we doing for time? We've done the whole of the lambda calculus, plus some extra bits. We should feel pretty smug.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Let's do a little practice exercise. Like a very short game of chess, now I've explained most of the rules.

;; So once upon a time there was this guy, believe it, called 'Hero of Alexandria'.

;; Or sometimes he seems to have been called 'Heron of Alexandria', like Hero was the short version, 
;; like he was sometimes called Jack and sometimes called John.

;; Who knows what his Christian name was?

;; Whatever, Hero invented the syringe, and the vending machine, and the steam engine, and the windmill, and the rocket, 
;; and the shortest path theory of reflection of light, and did some theatre stuff, 
;; and he was like Professor of War at the big library in Alexandria.

;; You get the impression that if Alexandria had lasted just a little bit longer, 
;; the whole industrial revolution would have kicked off right there, and the Romans would have walked on the moon in about AD400.

;; And we'd all be immortal, and live amongst the stars. So you should take the fall of the Roman Empire *very* personally.

;; And one of his things was a way of finding the square roots of numbers, 
;; which is so good that it was how people found square roots right up until the invention of the computer.

;; So I'm going to explain that method to you, and you're going to explain it to this computer, and then you can get the computer
;; to calculate square roots for you, really fast. And after that you're only a couple of steps away from cracking the 
;; Enigma codes and winning the second world war and inventing the internet and creating an artificial intelligence 
;; that will kill us all just 'cos it's got better things to do with our atoms. I'm not joking.

;; So careful.... What I've just given you is the first step on the path that leads to becoming a mighty and powerful wizard.
;; And with great power comes great something or other, you'll find it on the internet, so remember that.

;; PAUSE

;; So imagine you want to find the square root of 9. And you're a bit stuck, so you say to your friend, "What's the square root of nine?", and he says it's three.

;; How do you check?

(* 3 3)

;; Bingo. There's another way to check

(/ 9 3)

;; That's what it means to be the square root of something. If you divide the something by the square root, you get the square root back.

;; But what if your friend had said "err,.. 2 or something?"

(/ 9 2)

;; Notice that the number you put in is too low, but the number you got back is too high.

;; So Heron says, let's take the average.

;; So we need an average function

(define average (lambda (a b) (/ (+ a b) 2)))

(average 2 (/ 9 2))

;; three and a quarter, that's like a much better guess, it' like you'd found a cleverer friend.

;; so try again.

(average 3.25 (/ 9 3.25)) ; 3.009615...

;; and again 

(average 3.0096 (/ 9 3.0096)) ; 3.0000153..

(average 3.0000153 (/ 9 3.0000153)) ; 3.000000000039015

;; So you see this little method makes guesses at square roots of things into much better guesses.

;; Can you make a function which takes a guess at the square root of nine, and gives back a better guess?

(define improve-guess (lambda (guess) (average guess (/ 9 guess))))

;; I'd better show you how to format these little functions so that they're easier to read

(define improve-guess 
  (lambda (guess) 
    (average guess (/ 9 guess))))

;; The evaluator doesn't notice the formatting, and it makes it a bit more obvious what's getting replaced by what.

(improve-guess 4) ; 3 1/8
(improve-guess (improve-guess 4)) ; 3 1/400
(improve-guess (improve-guess (improve-guess 4))) ; 3 1/960800

;; We all know what the square root of nine is, let's look at a more interesting number, two. 
;; It's a bit of an open question whether 'the square root of two' is a number, or whether it's just a noise 
;; that people make with their mouths shortly after you show them a square and tell them about Pythagoras theorem.

;; Pythagoras used to have people killed for pointing out that you couldn't write down the square root of two.

;; I've got a bit of a confession to make. 

;; Someone's already explained to this computer how to find square roots

(sqrt 9)          ; so far so good!
(sqrt 2)          ; 1.4142135623730951   hmmm. let's check.

(square (sqrt 2)) ; 2.0000000000000004

;; So it turns out that this guy's just said, if you can't come up with the square root of two, just lie, and come up with something
;; that works, close as damnit. 

;; Which is like, bad practice, and tends to lead to Skynet-type behaviour in the long run.

;; So let's see what Hero would have said about it.

;; We need a new function that makes guesses better at being square roots of two.
;; It's a bit dirty, but let's just call that improve-guess as well.

;; That's called redefinition, or 'mutation', and it's ok when you're playing around, 
;; but it's a thing you should avoid when writing real programs, because, you know, Skynet issues.

;; Hell, no-one ever got more powerful by refraining from things.

(define improve-guess 
  (lambda (guess) 
    (average guess (/ 2 guess))))

;; Anyone make a guess? 

(improve-guess 1) ; 1 1/2

;; Any good?

(square (improve-guess 1)) ; 2 1/4

;; How wrong?

(- (square (improve-guess 1)) 2) ; 1/4

;; Could make a function out of that

(define wrongness (lambda (guess) (- 2 (square guess))))

(wrongness (improve-guess 1))
(wrongness (improve-guess (improve-guess 1)))
(wrongness (improve-guess (improve-guess (improve-guess 1))))
(wrongness (improve-guess (improve-guess (improve-guess (improve-guess 1)))))

;; So we're getting closer! When should we stop? Let's say when we're within 0.0000001

(define good-enough? (lambda (guess) (< (absolute-value (wrongness guess)) 0.0000001)))

(good-enough? (improve-guess (improve-guess (improve-guess (improve-guess 1)))))


(improve-guess (improve-guess (improve-guess (improve-guess 1))))

;; Now, we're doing a bit too much typing for my taste.

;; What we want to do is to say

;; I'll give you a guess. If it's good enough, just give it back. If it's not good enough, make it better AND TRY AGAIN.

;; This is the hard bit. We need to make a function that calls itself.

;; Go on, have a go

(define good-enough-guess 
  (lambda (guess)
    (if (good-enough? guess) guess
        (good-enough-guess (improve-guess guess)))))


(good-enough-guess 1) ; 1 195025/470832

;; I'll show you a trick now. 

(good-enough-guess 1.0) ; 1.4142135623746899

;; This is called 'contagion'. There are really two types of numbers.

;; Numbers that look like 432/123 are called 'exact', or 'vulgar fractions'
;; Numbers that look like 1.4142 are called 'inexact', or 'approximate', or 'floating point', or 'decimal fractions'.

;; The first type are the sort of numbers that children learn about in school, and that mathematicians use.

;; And the second type are the sort of numbers that engineers use, and they're actually quite a lot more complicated and fuzzy
;; than the exact type. They just sort of work like 'if it's very close, then it's good enough'. 
;; They keep about sixteen digits around, and if you want more than that, tough luck.

;; But for some purposes they're better, for instance they're easier to read, and it's a bit of a matter of taste.

;; If you multiply or add an inexact number to an exact number, the answer is always inexact.
;; You can't unapproximate something.

(/ 1 3)   ; 1/3
(/ 1.0 3) ; 0.3333333333333333

;; We all know that 1/3 isn't really 0.33333333333333

;; Mathematicians worry about that sort of thing. Engineers don't. Sometimes aeroplanes crash. Mostly they don't.



;; If we want to get a better guess for the square root of two, then we need to change our definition of what good-enough is:

(define good-enough? (lambda (guess) (< (absolute-value (wrongness guess)) 1/10000000000000000)))

(good-enough-guess 1) ; 1 259717522849/627013566048

; (good-enough-guess 1.0) ;; Ooh, it never stops. What is going on? 

(* 1.0 (good-enough-guess 1)) ; 1.4142135623730951

(sqrt 2) ; 1.4142135623730951

;; We know now, why the guy who told this computer how to find square roots maybe shaved a little bit.

;; But we know how to make answers that are as good as we like.

;; We can be really sloppy if we like:
(define good-enough? (lambda (guess) (< (absolute-value (wrongness guess)) 1/10)))

(good-enough-guess 1)
(good-enough-guess 1.0)

;; But all this mutation and redefinition is a bad habit to get into. It's a bit like lying. After a while, you forget what the truth
;; is, and because different bits of your program believe different things, you get into all sorts of trouble.










