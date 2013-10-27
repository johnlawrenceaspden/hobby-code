
;; But there are also some more things we want to talk about, and at
;; this point we leave maths for primary schools behind and move on to
;; the sort of things that you can only explain to teenagers.

;; How much zoom does an arrow represent?
;; Well, the magnification factor is the length of the arrow, which we can work out using pythagoras' theorem.

(defn zoom [[a b]]
  (Math/sqrt (+ (* a a) (* b b))))

(zoom arrow1) ;-> 5.0
(zoom arrow2) ;-> 5.0
(zoom arrow3) ;-> 1.004987562112089

;; And in fact, that works as you'd expect it to: 
(* (zoom arrow1) (zoom arrow2)) ;-> 25.0
(zoom (multiply-arrows arrow1 arrow2)) ;-> 25.0

;; Arrow multiplication makes the length of the product equal to the
;; product of the lengths of the things you're multiplying.

;; What about the twist?  Twist is the angle an arrow makes with the
;; line pointing east (or the positive half of the real numbers, if we
;; want to think that the real numbers are embedded in our arrows)

(defn twist [[a b]]
  (Math/atan2 b a))

;; Arrows which are also real numbers like 1, aka 1+0i, aka (1,0) have no twist.
(twist [1 0]) ;-> 0.0

;; An arrow which goes 1 east and 1 north
(twist [1 1]) ;-> 0.7853981633974483


;; Eek, scary, radians. Make them go away!!

(defn radians-to-degrees [x] (* 360 x (/ 1 2 Math/PI)))

(radians-to-degrees (twist [1 1])) ;-> 45.0 degrees

;; Actually, I rather like radians.

;; The key to radian angles is to think in terms of the circle constant tau

(def tau (* 2 Math/PI))

tau ;-> 6.283185307179586

;; Tau is how far it is round a circle, if the circle has radius one. 

;; So tau/8 is 'one eighth of a turn'

(/ tau 8)     ;-> 0.7853981633974483
(twist [1 1]) ;-> 0.7853981633974483

;; Radians would be a lovely, simple way to measure angles if only
;; we'd taken pi to be 'circumference over radius' rather than the
;; weird 'circumference over diameter'. So just use tau instead and
;; pretend we made the right choice way back when.

(defn radians-to-turns[x]
  (/ x tau))

(radians-to-turns (twist [1 1])) ;-> 0.125 ;; one eighth of a turn, see.

;; Now we can see what cos and sin (in radians) are really for:

;; If we travel one-eighth of the way (anticlockwise) round a circle (of radius one)

(Math/cos (/ tau 8)) ;-> 0.7071067811865476
(Math/sin (/ tau 8)) ;-> 0.7071067811865475

;; Then we're as far north as we are east (about 7/10ths north and east)

;; If we go 1/100th of the way round a circle of radius one
(Math/cos (/ tau 100)) ;-> 0.9980267284282716
(Math/sin (/ tau 100)) ;-> 0.06279051952931337

;; then our east-ness has hardly changed, but we've moved about tau/100 north.

;; Well, yes. That was the plan. Move 1/100th of the distance
;; round. That's not very far, so you've been pretty much going due
;; north all the time. So you're roughly tau/100 north of where you
;; started.


;; We can make an arrow that is some part of the way round

(defn make-arrow-no-zoom [angle-in-radians]
  [(Math/cos angle-in-radians) (Math/sin angle-in-radians)])


(do (make-blackboard "One Eighth of the Way Round" 2)
    (draw-arrow (make-arrow-no-zoom (/ tau 8))))


;; It feels like we should add this zoom and turn thing to our arrow-printing routine now:
(defn print-arrow [[a b :as arrow]]
   (str "the pair (" a "," b "), "
        "also known as the complex number " a "+"b"i, "
        "also known as the arrow " (cond (> a 0) (str a " north") (= a 0) "" :else (str (- a) "south"))
        " and " (cond (> b 0) (str b " east") (= b 0) "" :else (str (- b) "west"))
        ", also known as zoom " (zoom arrow) " twist " (radians-to-turns (twist arrow)) "tau"))

(print-arrow [0,0]) ;-> "the pair (0,0), also known as the complex number 0+0i, also known as the arrow  and , also known as zoom 0.0 twist 0.0tau"
(print-arrow [1,0]) ;-> "the pair (1,0), also known as the complex number 1+0i, also known as the arrow 1 north and , also known as zoom 1.0 twist 0.0tau"
(print-arrow [0,1]) ;-> "the pair (0,1), also known as the complex number 0+1i, also known as the arrow  and 1 east, also known as zoom 1.0 twist 0.25tau"

;; That could totally use some tidying up

;; In fact, printing these damned things turns out to be the most complicated thing about them.

(defn print-arrow [[a b :as arrow]]
   (str "the pair (" a "," b "), "
        "also known as the complex number " 
        (cond (and (= a 0) (= b 0)) (str "0 also known as zero")
              (and (= a 0) (= b 1)) (str "i also known as the unit north arrow")
              (and (= a 0) (= b -1)) (str "-i also known as the unit south arrow")
              (and (= a 1) (= b 0)) (str "1 also known as the unit east arrow")
              (and (= a -1) (= b 0)) (str "-1 also known as the unit west arrow")
              (= a 0) (if (> b 0) (str b "i also known as the arrow " b "north") (str b "i also known as the arrow " b " south"))
              (= b 0) (if (> a 0) (str a " also known as the arrow " a "east") (str a " also known as the arrow " a " west"))
              :else (cond (and (> a 0) (> b 0)) (str a "+" b "i, also known as the arrow " a " east and " b " north")
                          (and (< a 0) (> b 0)) (str a "+" b "i, also known as the arrow " (- a) " west and " b " north")
                          (and (> a 0) (< b 0)) (str a "-" (- b) "i, also known as the arrow " a " east and " (- b) " south")
                          (and (< a 0) (< b 0)) (str a "-" (- b) "i, also known as the arrow " (- a) " west and " (- b) " south")))
        ", also known as zoom " (zoom arrow) " twist " (radians-to-turns (twist arrow)) "tau"))

(print-arrow [0,0]) ;-> "the pair (0,0), also known as the complex number 0 also known as zero, also known as zoom 0.0 twist 0.0tau"
(print-arrow [1,0]) ;-> "the pair (1,0), also known as the complex number 1 also known as the unit east arrow, also known as zoom 1.0 twist 0.0tau"
(print-arrow [0,1]) ;-> "the pair (0,1), also known as the complex number i also known as the unit north arrow, also known as zoom 1.0 twist 0.25tau"
(print-arrow [3, -4]) ;-> "the pair (3,-4), also known as the complex number 3-4i, also known as the arrow 3 east and 4 south, also known as zoom 5.0 twist -0.14758361765043326tau"
(print-arrow [3, 4]) ;-> "the pair (3,4), also known as the complex number 3+4i, also known as the arrow 3 east and 4 north, also known as zoom 5.0 twist 0.14758361765043326tau"
(print-arrow [-3, 4]) ;-> "the pair (-3,4), also known as the complex number -3+4i, also known as the arrow 3 west and 4 north, also known as zoom 5.0 twist 0.35241638234956674tau"
(print-arrow [-3, -4]) ;-> "the pair (-3,-4), also known as the complex number -3-4i, also known as the arrow 3 west and 4 south, also known as zoom 5.0 twist -0.35241638234956674tau"



;; How about making an arrow that is 1/100th of a full turn, and repeatedly multiplying 
;; (make-arrow-no-zoom (/ tau 8)) by it, and plotting all the results?

(do (make-blackboard "One Hundred Hundreths of Tau" 2)
    (draw-arrow (make-arrow-no-zoom (/ tau 8)))
    (doseq [i (take 100 
                    (reductions multiply-arrows 
                                (make-arrow-no-zoom (/ tau 8)) 
                                (repeat (make-arrow-no-zoom (/ tau 100)))))]
      (draw-arrow i)))


;; OK, what if we want some sort of zooming factor in our arrows?
;; Zooming factors without turns are just real numbers, so we can multiply a pure zoom by a pure turn
(defn make-arrow [angle-in-radians zoom]
  (multiply-arrows 
   [zoom 0] 
   (make-arrow-no-zoom angle-in-radians)))

(def slight-zoom-and-twist (make-arrow (/ tau 100) 1.01))

(do (make-blackboard "Another Spiral" 2)
    (doseq [i (take 500 
                    (reductions multiply-arrows [1/10,0] (repeat slight-zoom-and-twist)))]
      (draw-arrow i))
    (ink red)
    (draw-arrow slight-zoom-and-twist))

(def slight-shrink-and-anti-twist (make-arrow (- (/ tau 100)) (/ 1.01)))

(do (make-blackboard "Spiral In" 2)
    (doseq [i (take 500 
                    (reductions multiply-arrows [2,0] (repeat slight-shrink-and-anti-twist)))]
      (draw-arrow i))
    (ink red)
    (draw-arrow slight-shrink-and-anti-twist))
  
;; I'm starting to feel the need for a length scale on these arrow diagrams, let's plot the unit circle

(do (make-blackboard "Unit Circle" 2)
    (doseq [[x y] (reductions multiply-arrows [1,0] (repeat 200 (make-arrow-no-zoom (/ tau 200))))]
      (plot x y)))

;; In fact let's have that on all blackboards from now on
(defn make-blackboard [title size]
  (create-window title 400 400 white black (- size) size (- size) size)
  (axes)
  (doseq [[x y] (reductions multiply-arrows [1,0] (repeat 200 (make-arrow-no-zoom (/ tau 200))))]
    (plot x y))
  (ink yellow))

;; Did you notice that slight-zoom-and-twist and slight-shrink-and-anti-twist were opposite operations?
(multiply-arrows slight-shrink-and-anti-twist slight-zoom-and-twist) ;-> [1.0 0.0]

(do 
  (make-blackboard "With Scale" 1.1)
  (draw-arrow slight-shrink-and-anti-twist)
  (draw-arrow slight-zoom-and-twist)
  (ink red)
  (draw-arrow (multiply-arrows slight-shrink-and-anti-twist slight-zoom-and-twist)))


;; In general, to find the inverse arrow, the thing that will undo the
;; thing, you shrink instead of zooming, and you twist the other way

(defn inverse [arrow]
  (make-arrow (- (twist arrow)) (/ (zoom arrow))))

(do 
  (make-blackboard "Inverse" 3)
  (draw-arrow arrow1)
  (draw-arrow (inverse arrow1))
  (ink red)
  (draw-arrow (multiply-arrows arrow1 (inverse arrow1))))

;; There's also the idea of a conjugate, which is the opposite twist but the same zoom

(defn conjugate [arrow]
  (make-arrow (- (twist arrow)) (zoom arrow))))

(conjugate arrow1) ;-> [3.0000000000000004 -3.9999999999999996]

(do 
  (make-blackboard "Conjugate" 10)
  (draw-arrow arrow1)
  (ink green)
  (draw-arrow (conjugate arrow1))
  (ink red)
  (draw-arrow (multiply-arrows arrow1 (conjugate arrow1))))

;; Just kidding
(defn conjugate [[a b]]
  [a (- b)])

;; This is what it really looks like:
(do 
  (make-blackboard "Conjugate" 10)
  (draw-arrow arrow1)
  (ink green)
  (draw-arrow (conjugate arrow1))
  (ink red)
  (draw-arrow (multiply-arrows arrow1 (conjugate arrow1))))

;; Either way, if you multiply an arrow by its conjugate, you end up with 
;; an arrow which is one of the real numbers, whose zoom factor is the square of the zoom factor of the arrow

(zoom arrow1) ;-> 5.0
(zoom (multiply-arrows arrow1 (conjugate arrow1))) ;-> 25.0

(do 
  (make-blackboard "Multiply by Conjugate" 25)
  (draw-arrow arrow1)
  (ink green)
  (draw-arrow (conjugate arrow1))
  (ink red)
  (draw-arrow (multiply-arrows arrow1 (conjugate arrow1))))

;; Well, anyway, blah, blah blah, are we all happy with how the arrows
;; aka complex numbers work now? All the usual properties seem pretty obvious.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Just to clear up something that might have been bothering you, why
;; did we choose that multiplication rule which looked weird but
;; turned out to work well and be about adding angles?

;; Recall the trig identities:
;; cos(a+b) = cos(a)cos(b) - sin(a)sin(b)
;; sin(a+b) = cos(a)sin(b) + sin(a)cos(b)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; The whole point of this so far has been:

;; Nobody needs to make up an 'imaginary' number in order to 'satisfy the equation x*x+1=0'.

;; You just think about these arrows, and how you multiply and add them.

;; The arrows are clearly real, not-scary things that actually exist
;; and are quite friendly and fun, and are useful for thinking about
;; photoshop and drawing spirally arrow things.

;; And if you play with them for a bit you realize that:

;; (a) they behave a lot like numbers in that they can be added and
;; multiplied and subtracted and divided and all the distributive,
;; commutative, associative properties that you've grown up with also
;; apply to the arrows.

;; (b) they've got a copy of the numbers that you already know inside
;; them. So in particular, there's an arrow (which means half-turn, no
;; zoom) which behaves awfully like -1 behaves.

;; (c) there's a thing (actually two things, because a clockwise
;; quarter turn done twice is also a half-turn) which, when squared,
;; becomes the thing that you've noticed acts like -1.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The traditional presentation of the complex numbers goes something like:

;; It's not nice that there's no solution to x^2-1=0, so let's imagine
;; what, if such a thing existed, it would have to be like.

;; Understandably, this leaves most of the people who see it rigid
;; with terror and suspicion.

;; This presentation has the virtue of historical accuracy, in that
;; historically, people viewed 'imaginary numbers' with suspicion and
;; terror.

;; But it doesn't have the virtue of historical accuracy in the sense
;; that it's actually true.

;; Nobody has ever had the slightest problem with x^2-1 not having a
;; solution. It would be a bit freaky if it did. It makes about as
;; much sense as demanding that the colour blue has a square root, and
;; adding that to the number system and seeing what happens.

;; However there were some weird things going on when people were
;; trying to solve cubic equations, and those persuaded people that if
;; they pretended that negative numbers had square roots, they could
;; sometimes get them to cancel out and get real answers to cubics
;; that way. And since these tricks with 'imaginary numbers' seemed to
;; usually work, people got to not noticing how odd it was to
;; manipulate something that didn't exist.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This arrow presentation on the other hand, goes something like:

;; Here are some things that we made up that behave in fairly
;; straightforward ways.  They'll probably turn out to be useful for
;; describing displacement and rotation and zooming.

;; Hidden inside them is a structure which is just like the numbers we already know.

;; Later on we'll see:

;; As it happens, we can define polynomials here, and guess what? They
;; always have roots. That's kind of cute, if polynomials are your thing.

;; But actually that's not what the complex numbers are about. They
;; are about rotation and growth and decay and geometry and periodic
;; processes and simple harmonic motion and oscillations and
;; frequencies and oh yes, nearly forgot, they turn out to be absolutely
;; essential for describing the structure of the universe.

;; And a lot of this cool stuff is rendered much less easy to
;; understand than it should be by this spooky aura that's left over
;; from the eighteenth century or whenever, when people were so
;; nervous of 'imaginary numbers' that even the great mathematician
;; Euler kept making stupid mistakes when he talked about them.

;; Oh, yes, Euler's Identity e^(i*pi) = -1

;; From Wikipedia:

;; Euler's formula is ubiquitous in mathematics, physics, and engineering. 
;; The physicist Richard Feynman called the equation "our jewel" and "one of the most remarkable, almost astounding, formulas in all of mathematics."
;; A poll of readers conducted by The Mathematical Intelligencer named Euler's identity as the "most beautiful theorem in mathematics".
;; Another poll of readers that was conducted by Physics World in 2004 chose Euler's identity tied with Maxwell's equations (of electromagnetism) as the "greatest equation ever".

;; Well, alright, it's pretty neat. I like Euler's Identity.

;; It turns out to mean 'If you turn for the time it takes you to do half a turn, you'll be pointing backwards.'

;; Profound, huh?











































;; How long is that thing anyway? Remember Pythagoras?
(defn length [arrow]
  (Math/sqrt (reduce + (map * arrow arrow))))

(length (multiply-arrows arrow1 arrow2)) ;-> 25.0

;; Oooh. That's a suspiciously round number. 

;; How long were the originals?

(length arrow1) ;-> 5.0
(length arrow2) ;-> 5.0

;; So our multiplication rule seems to multiply arrow lengths

;; What about the direction of the red arrow? 











;; Well notice that arrow1 is turned a bit anticlockwise from (1,0), the arrow pointing east

(ink green) ;hard to see
(draw-arrow [1 0])

;; And that arrow2 is turned a bit clockwise from there.

;; And think of the arrows as meaning 'a zoom and a twist'.

;; How much zoom? The length of the arrow.

;; How much turn? The amount the arrow is turned away from (1,0), our
;; 'favourite arrow', which means 'do nothing', or 'leave everything
;; the same'.

;; So if we think that arrow1 might represent zoom by a factor of 5
;; and turn anticlockwise by something like 60 degrees.

;; And we think that arrow 2 might represent zoom by a factor of 5 and
;; turn clockwise by a bit less.

;; Then it doesn't look too strange that arrow1*arrow2 might mean do
;; both, one after the other, and end up meaning 'zoom by a factor of
;; 25 and turn a bit anticlockwise'

;; Which it looks as though the red arrow might well mean.

;; Let's test our idea with some smaller arrows.

;; Arrow 3 will be [1,0.1]
(def arrow3 [1,0.1])

(cls)
(ink white)
(axes)
(ink yellow)
(draw-arrow arrow3)


;; Now I hope you'll agree with me that while this is a bit strange,
;; it's all perfectly easy to understand, and there's nothing magical going on.

;; And it gives us a nice way to talk about plane geometry.

;; We have arrows, which we can think of either as 'displacements', using our addition rule,
;; or 'turns and zooms', using our multiplication rule.











;; The traditional explanation goes something like:

;; 'We'd like to be able to solve x^2-1=0', but we can't, so we just
;; pretend that we can and treat the answer as a number, and nothing
;; goes wrong and it turns out to be useful.

;; Now to be honest, I was once deeply immersed in the dark arts of
;; pure mathematics, and if you're interested in logic and set theory
;; and the consistency of axioms and models and all this and that,
;; then there is a way in which that sort of idea actually makes sense.

;; But you will not meet many people who understand it, and so it just
;; sounds like witchcraft and ad-hockery.

