;; Feynman's Arrows III: Conjugates, Pythagoras, Zoom, Twist, Radians, Tau, 
;; requires [simple-plotter "0.1.2"] 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Here's some code from the previous post

(use 'simple-plotter)

(defn make-blackboard [title size]
  (create-window title 400 400 white black (- size) size (- size) size)
  (axes)
  (ink yellow))

(defn draw-offset-arrow [[a b][c d]]
  (let [headx (+ a c) 
        heady (+ b d)]
    (line a b headx heady)
    (line headx heady (+ headx (* -0.086 c) (* -0.05 d)) (+ heady (*  0.05 c) (* -0.086 d)))
    (line headx heady (+ headx (* -0.086 c) (*  0.05 d)) (+ heady (* -0.05 c) (* -0.086 d)))))

(defn draw-arrow [[a b]] (draw-offset-arrow [0 0] [a b]))

(defn add-arrows[[a b][c d]]   
  [(+ a c) (+ b d)])

(defn multiply-arrows[[a b][c d]]
  [(- (* a c) (* d b)) (+ (* a d) (* c b))])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The hardest thing about complex numbers is printing them out. 
;; It's also monumentally uninteresting, so I'm just going to give a print routine 

(defn print-arrow [[a b]]
  (str "the pair (" a "," b "), also known as "
       (cond (and (= a 0) (= b 0)) (str "zero")
             (and (= a 0) (= b 1))  (str "the imaginary number i also known as the unit north arrow")
             (and (= a 0) (= b -1)) (str "the imaginary number -i also known as the unit south arrow")
             (and (= a 1) (= b 0))  (str "the real number 1 also known as the unit east arrow")
             (and (= a -1) (= b 0)) (str "the real number -1 also known as the unit west arrow")
             (= a 0) (str "the imaginary number " b "i "
                          (if (> b 0) 
                            (str "also known as the arrow " b " north")
                            (str "also known as the arrow " (- b) " south")))
             (= b 0) (str "the real number " a
                          (if (> a 0) 
                            (str  " also known as the arrow " a " east") 
                            (str  " also known as the arrow " (- a) " west")))
             :else (cond (and (> a 0) (> b 0)) (str a "+" b "i, also known as the arrow " a " east and " b " north")
                         (and (< a 0) (> b 0)) (str a "+" b "i, also known as the arrow " (- a) " west and " b " north")
                         (and (> a 0) (< b 0)) (str a "-" (- b) "i, also known as the arrow " a " east and " (- b) " south")
                         (and (< a 0) (< b 0)) (str a "-" (- b) "i, also known as the arrow " (- a) " west and " (- b) " south")))))



(def arrow1 [3,4]) 
(def arrow2 [4,-3])
(def arrow3 [1,1/10])

(print-arrow arrow1) ;-> "the pair (3,4), also known as 3+4i, also known as the arrow 3 east and 4 north"
(print-arrow arrow2) ;-> "the pair (4,-3), also known as 4-3i, also known as the arrow 4 east and 3 south"
(print-arrow arrow3) ;-> "the pair (1,1/10), also known as 1+1/10i, also known as the arrow 1 east and 1/10 north"

;; I really do think that everything up to this point could be done in
;; primary schools.  Although I would way hold off on the whole
;; 'imaginary number' terminology until they are old enough that it
;; just seems like another silly thing people used to do in the past.

;; There's one more thing that I think little children might find easy
;; and interesting, and that's the idea of the complex conjugate.

;; If it goes upward, make one like it that goes down. If it goes down, make it go up instead.

(defn conjugate [[a b]]
  [a (- b)])

(conjugate arrow1) ;-> [3 -4]

;; This is what it looks like:
(do 
  (make-blackboard "Conjugate" 10)
  (draw-arrow arrow1)
  (ink green)
  (draw-arrow (conjugate arrow1)))

;; Here are lots. Green ones go with yellow ones
(do 
  (make-blackboard "Various Conjugate Pairs" 10)
  (dotimes[i 10]
    (let [x (- (rand-int 21) 10)
          y (- (rand-int 21) 10)]
          (ink yellow)
          (draw-arrow [x y])
          (ink green)
          (draw-arrow (conjugate [x y])))))


;; If an arrow represents a zoom and a turn, then its conjugate represents the same zoom, but turning the other way.

;; And that means that if you multiply an arrow by its conjugate, the turns will cancel out, but the zoom will be done twice.

(do 
  (make-blackboard "Multiply by Conjugate" 25)
  (draw-arrow arrow1)
  (ink green)
  (draw-arrow (conjugate arrow1))
  (ink red)
  (draw-arrow (multiply-arrows arrow1 (conjugate arrow1))))

;; So we can say:

(defn zoom-squared [arrow]
  (multiply-arrows arrow (conjugate arrow)))

(zoom-squared arrow1) ;-> [25 0]
(zoom-squared arrow1) ;-> [25 0]

;; Another way to calculate zoom-squared is
(defn zoom-squared [[x y]]
  [(+ (* x x) (* y y)), 0])

(zoom-squared arrow1) ;-> [25 0]

;; Remember that [25,0] is just another name for the real number 25
(print-arrow [25,0]) ;-> "the pair (25,0), also known as the real number 25 also known as the arrow 25 east"

;; We've just, quite by accident, proved Pythagoras' Theorem.  Which
;; means that it's implied by the idea of zooming and twisting. I
;; wouldn't mention that to the kids.


;; In fact, we've probably exhausted the sorts of things you could do at primary schools with Complex Numbers

;; You might get away with subtraction
(defn subtract-arrows [a1 a2]
  (add-arrows a1 (multiply-arrows [-1 0] a2)))

(subtract-arrows arrow1 arrow2) ;-> [-1 7]

;; Or maybe just directly
(defn subtract-arrows [[a b][c d]]
  [(- a c)(- b d)])


(do 
  (make-blackboard "Subtraction" 7)
  (draw-arrow arrow1)
  (draw-arrow arrow2)
  (ink green)
  (draw-offset-arrow arrow2 (subtract-arrows arrow1 arrow2))
  (ink red)
  (draw-arrow (subtract-arrows arrow1 arrow2)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; But there are also some more things we want to talk about, and at
;; this point we leave maths for primary schools behind and move on to
;; the sort of things that you can only explain to teenagers.


;; How much zoom does an arrow represent?
;; Well, the magnification factor is the length of the arrow, which we can work out using Pythagoras' theorem.

(defn zoom [[a b]]
  (Math/sqrt (+ (* a a) (* b b))))

(zoom arrow1) ;-> 5.0
(zoom arrow2) ;-> 5.0
(zoom arrow3) ;-> 1.004987562112089



;; That works as you'd expect it to: 
(* (zoom arrow1) (zoom arrow2)) ;-> 25.0
(zoom (multiply-arrows arrow1 arrow2)) ;-> 25.0

;; Arrow multiplication makes the length of the product equal to the
;; product of the lengths of the things you're multiplying. As we set
;; it up to do.

;; What about the twist?  Twist is the angle an arrow makes with the
;; line pointing east (or the positive half of the real numbers, if we
;; want to think that the real numbers are embedded in our arrows)

;; To work out the angle of the twist represented by an arrow we use 
;; exactly the same method that we'd use for vectors, the arc-tangent

(defn twist [[a b]]
  (Math/atan2 b a))

;; Arrows which are also real numbers like 1, aka 1+0i, aka (1,0) have no twist.
(twist [1 0]) ;-> 0.0

;; An arrow which goes 1 east and 1 north, however, does represent a twist
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

;; It feels like we should add this twist and zoom thing to our printed representation of an arrow

(defn print-arrow [[a b :as arrow]]
  (str "the pair (" a "," b "), also known as "
       (cond (and (= a 0) (= b 0)) (str "zero")
             (and (= a 0) (= b 1))  (str "the imaginary number i, also known as the unit north arrow")
             (and (= a 0) (= b -1)) (str "the imaginary number -i, also known as the unit south arrow")
             (and (= a 1) (= b 0))  (str "the real number 1, also known as the unit east arrow")
             (and (= a -1) (= b 0)) (str "the real number -1, also known as the unit west arrow")
             (= a 0) (str "the imaginary number " b "i, "
                          (if (> b 0) 
                            (str "also known as the arrow " b " north")
                            (str "also known as the arrow " (- b) " south")))
             (= b 0) (str "the real number " a
                          (if (> a 0) 
                            (str  ", also known as the arrow " a " east") 
                            (str  ", also known as the arrow " (- a) " west")))
             :else (cond (and (> a 0) (> b 0)) (str a "+" b "i, also known as the arrow " a " east and " b " north")
                         (and (< a 0) (> b 0)) (str a "+" b "i, also known as the arrow " (- a) " west and " b " north")
                         (and (> a 0) (< b 0)) (str a "-" (- b) "i, also known as the arrow " a " east and " (- b) " south")
                         (and (< a 0) (< b 0)) (str a "-" (- b) "i, also known as the arrow " (- a) " west and " (- b) " south")))
       ", also known as zoom " (zoom arrow) " twist " (radians-to-turns (twist arrow)) "tau"))

;; The fact that it doesn't make it very much longer shows that it's a nice natural representation of a pair

(print-arrow [0,0]) ;-> "the pair (0,0), also known as zero, also known as zoom 0.0 twist 0.0tau"
(print-arrow [1,0]) ;-> "the pair (1,0), also known as the real number 1, also known as the unit east arrow, also known as zoom 1.0 twist 0.0tau"
(print-arrow [0,1]) ;-> "the pair (0,1), also known as the imaginary number i, also known as the unit north arrow, also known as zoom 1.0 twist 0.25tau"
(print-arrow [3, -4]) ;-> "the pair (3,-4), also known as 3-4i, also known as the arrow 3 east and 4 south, also known as zoom 5.0 twist -0.14758361765043326tau"
(print-arrow [3, 4]) ;-> "the pair (3,4), also known as 3+4i, also known as the arrow 3 east and 4 north, also known as zoom 5.0 twist 0.14758361765043326tau"
(print-arrow [-3, 4]) ;-> "the pair (-3,4), also known as -3+4i, also known as the arrow 3 west and 4 north, also known as zoom 5.0 twist 0.35241638234956674tau"
(print-arrow [-3, -4]) ;-> "the pair (-3,-4), also known as -3-4i, also known as the arrow 3 west and 4 south, also known as zoom 5.0 twist -0.35241638234956674tau"


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Now we can see what cos and sin (in radians) are:

;; You give them a twist, and they'll give you an arrow

;; If we travel one-eighth of the way (anticlockwise) round a circle (of radius one)

(Math/cos (/ tau 8)) ;-> 0.7071067811865476
(Math/sin (/ tau 8)) ;-> 0.7071067811865475

;; Then we're as far north as we are east (about 7/10ths north and 7/10ths east)

;; If we go 1/100th of a turn round a circle of radius one
(Math/cos (/ tau 100)) ;-> 0.9980267284282716
(Math/sin (/ tau 100)) ;-> 0.06279051952931337

;; then our east-ness has hardly changed, but we've moved about tau/100 north.

;; Well, yes. That was the plan. Move 1/100th of the distance
;; round. That's not very far, so you've been pretty much going due
;; north all the time. So you're roughly tau/100 north of where you
;; started.

;; That's why sin(x) approximates x for small x

;; We can make an arrow (length one) that is some part of the way round using cos and sin

(defn make-unit-arrow [twist]
  [(Math/cos twist) (Math/sin twist)])


(do (make-blackboard "One Eighth of the Way Round" 2)
    (draw-arrow (make-unit-arrow (/ tau 8))))


;; How about making an arrow that is 1/100th of a full turn, and repeatedly multiplying 
;; it into something , and plotting all the results?

(do (make-blackboard "One Hundred Hundreths of Tau" 2)
    (let [start (make-unit-arrow (/ tau 8))]
      (draw-arrow start)
      (doseq [i (reductions multiply-arrows 
                            start 
                            (repeat 100 (make-unit-arrow (/ tau 100))))]
        (draw-arrow i))))


;; OK, what if we want some sort of zooming factor in our arrows as well?
;; Zooming factors without turns are just real numbers, so we can multiply a pure zoom by a pure turn
(defn make-arrow [angle-in-radians zoom]
  (multiply-arrows 
   [zoom 0] 
   (make-unit-arrow angle-in-radians)))

;; We can make a lovely spiral this way
(def slight-zoom-and-twist (make-arrow (/ tau 100) 1.01))

(do (make-blackboard "Another Spiral" 2)
    (doseq [i (take 500 
                    (reductions multiply-arrows [1/10,0] (repeat slight-zoom-and-twist)))]
      (draw-arrow i))
    (ink red)
    (draw-arrow slight-zoom-and-twist))

;; Alternatively, if we take a slight twist the other way and shrink a bit
(def slight-shrink-and-anti-twist (make-arrow (- (/ tau 100)) (/ 1.01)))

;; Then we can make an inward spiral
(do (make-blackboard "Spiral In" 2)
    (doseq [i (take 500 
                    (reductions multiply-arrows [2,0] (repeat slight-shrink-and-anti-twist)))]
      (draw-arrow i))
    (ink red)
    (draw-arrow slight-shrink-and-anti-twist))
  
;; I'm starting to feel the need for a length scale on these arrow diagrams, let's plot the unit circle

(do (make-blackboard "Unit Circle" 2)
    (doseq [[x y] (reductions multiply-arrows [1,0] (repeat 200 (make-unit-arrow (/ tau 200))))]
      (plot x y)))

;; In fact let's have that on all blackboards from now on
(defn make-blackboard [title size]
  (create-window title 400 400 white black (- size) size (- size) size)
  (axes)
  (doseq [[x y] (reductions multiply-arrows [1,0] (repeat 200 (make-unit-arrow (/ tau 200))))]
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


;; Or entirely equivalently, we can make the inverse by using the conjugate
(defn inverse [arrow] 
  (multiply-arrows (conjugate arrow)
                   [ (/ (zoom (multiply-arrows arrow (conjugate arrow)))) 0]))


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
;; sometimes get them to cancel out and get answers to cubics
;; that way. And since these tricks with 'imaginary numbers' seemed to
;; usually work, people got to not noticing how odd it was to
;; manipulate something that didn't exist.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This arrow presentation on the other hand, goes something like:

;; Here are some things that we made up that behave in fairly
;; straightforward ways.  They'll probably turn out to be useful for
;; describing moving around and rotating and zooming.

;; Hidden inside them is a structure which is just like the numbers we
;; already know.  

;; And in fact we now realize all along that 3 has had dual character
;; of meaning move 3 to the right, and 'triple', and -3 has always
;; meant both 'move 3 to the left' and 'triple and reverse'

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
;; Euler kept making mistakes when he wrote about them.

;; Oh, yes, Euler's Identity e^(i*pi) = -1

;; From Wikipedia:

;; Euler's formula is ubiquitous in mathematics, physics, and engineering. 
;; The physicist Richard Feynman called the equation "our jewel" and "one of the most remarkable, almost astounding, formulas in all of mathematics."
;; A poll of readers conducted by The Mathematical Intelligencer named Euler's identity as the "most beautiful theorem in mathematics".
;; Another poll of readers that was conducted by Physics World in 2004 chose Euler's identity tied with Maxwell's equations (of electromagnetism) as the "greatest equation ever".

;; Well, alright, it's pretty neat. I like Euler's Identity.

;; It turns out to mean 'If you turn for the time it takes you to do half a turn, you'll be pointing backwards.'

;; Profound, huh?





;; FOOTNOTES

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Just to clear up something that might have been bothering you, why
;; did we choose that multiplication rule which looked weird but
;; turned out to work well and be about adding angles and multiplying lengths?

;; Well, we started off with the idea of combining zooms and twists geometrically.

;; And so we know that i * i is -1. Two quarter turns makes a half turn.

;; But we can also get the distributive law a*(b+c)=a*b+b*c from our geometrical definition of multiplication
;; Just because rotations and scalings preserve paralellograms

(do 
  (make-blackboard "Multiplication is Distributive" 6)
  (let [a [1,2]
        b [2,1]
        c [-0.7,1]
        aplusb (add-arrows a b)
        ca (multiply-arrows c a)
        cb (multiply-arrows c b)
        caplusb (multiply-arrows c aplusb)]
    (draw-arrow a)
    (draw-arrow b)
    (ink red)
    (draw-arrow aplusb)
    (ink green)
    (draw-offset-arrow a b)
    (draw-offset-arrow b a)
    (ink cyan)
    (draw-arrow c)
    (ink magenta)
    (draw-arrow ca)
    (draw-arrow cb)
    (draw-arrow caplusb)
    (draw-offset-arrow ca cb)
    (draw-offset-arrow cb ca)))


;; So that means that if  multiplication and addition are defined geometrically
;; Then  (a+ib)*(c+id) is forced to be equal to (ac-bd)+i(ad+bc).

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; We actually don't need trigonometry to define inverses and division
;; I came quite close to putting them in the 'primary school' section.

;; But although once we've got the conjugate we've got the tools for
;; division and inverses:
(defn inverse-arrow [arrow]
  (let [[length-squared _] (zoom-squared arrow)
        shrink-squared (/ length-squared)]
    (multiply-arrows [shrink-squared 0] (conjugate arrow))))

;; I'm not at all confident that I could explain that to children.

;; And the version where the inverse is defined by shrink and twist the
;; other way is much nicer to think about. Still, they both work and
;; they're entirely equivalent.

(do 
  (make-blackboard "Inverse Arrows" 5)
  (draw-arrow arrow1)
  (ink green)
  (draw-arrow (inverse-arrow arrow1))
  (ink red)
  (draw-arrow (multiply-arrows arrow1 (inverse-arrow arrow1))))

(defn divide-arrows [a1 a2]
  (multiply-arrows a1 (inverse-arrow a2)))


(do 
  (make-blackboard "Division" 5)
  (draw-arrow arrow1)
  (draw-arrow arrow2)
  (ink green)
  (draw-arrow (divide-arrows arrow1 arrow2))
  (draw-arrow (divide-arrows arrow2 arrow1)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; You might remember the trig identities:

;; cos(a+b) = cos(a)cos(b) - sin(a)sin(b)
;; sin(a+b) = cos(a)sin(b) + sin(a)cos(b)

;; They're just our multiplication rule in disguise. In fact, as is
;; the case with many trig identities, by far the easiest way to
;; understand why they're true is to think about complex numbers.

;; Our multiplication rule has taken the essence of plane geometry and
;; baked it into a number system for us.

;; And it turns out that that number system is the basis of reality itself.
