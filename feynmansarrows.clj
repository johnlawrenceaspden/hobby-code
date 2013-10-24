;; Feynman's Arrows : What are the Complex Numbers?

;; I've lost count of the number of times I've met physicists and even
;; professional mathematicians who think there's something a bit
;; spooky and unreal about the complex numbers.

;; There isn't.

;; Imagine that you're sitting in front of a photo-manipulation
;; program, and you're able to enlarge a photograph by a factor of
;; 2, and rotate it 30 degrees clockwise.

;; I don't think anyone is going to find that spooky. If you do, go
;; find a photo manipulation program and do it a couple of times.

;; If you do it twice, then you'll find that that gives you the same
;; effect as enlarging the photo by a factor of four and rotating it
;; 60 degrees clockwise.

;; In fact, how would we get our original photo back? My first guess
;; would be that I should shrink it by a factor of four and rotate it
;; 60 degrees anticlockwise. And that's the right answer. Go and try
;; it if it's not obvious.

;; And if you have your head round that, then you understand the
;; complex numbers.

;; If the mathematicians of the 16th century had been thinking about
;; how to use photoshop, instead of worrying about how to solve cubic
;; equations, then they'd have come up with the complex numbers in
;; about fifteen minutes flat, and they'd have thought they were the
;; most obvious thing in the world, and it would never have occurred
;; to them to talk about 'imaginary numbers', and an awful lot of
;; terror and confusion would have been avoided over the years.

;; And if the complex numbers had been found that way, then I think
;; we'd teach them to little children at about the same time we teach
;; them about fractions, and well before we teach them about the
;; really weird stuff like the square roots of two.

;; And the little children would have no problem at all with them, 
;; and they would think that they were fun, and easy.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; In his wonderful little book QED, Richard Feynman managed to
;; explain the soul of Quantum Field Theory without using the complex
;; numbers at all.

;; Instead he talks about little arrows, which rotate like the hands
;; on a stopwatch, and occasionally he needs to add them up and rotate
;; them to work out what light and electrons are going to do.

;; Before we talk about the complex numbers, let's think about
;; Feynman's arrows.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GRAPHICS CODE

;; I'd like to draw some pictures.  And I'd like to use my
;; 'simple-plotter' library, which is on clojars:
;; https://clojars.org/simple-plotter

;; This is probably the most complicated bit of this post, but it's
;; just graphics, and it doesn't matter in the slightest, so feel free
;; to skip over it unless you're interested in how to draw little
;; arrows on a computer.

;; You can add this:
;; [simple-plotter "0.1.2"] 

;; to your project file and restart everything, or if you are wise and
;; have the incomparable pomegranate on your classpath you can do:

(require 'cemerick.pomegranate)
(cemerick.pomegranate/add-dependencies 
 :coordinates '[[simple-plotter "0.1.2"]] 
 :repositories {"clojars" "http://clojars.org/repo"})

;; Either way, once the library is added to classpath
(use 'simple-plotter)

;; We can use it to draw arrows with tiny heads on them, so:
(defn draw-offset-arrow [[a b][c d]]
  (let [headx (+ a c) 
        heady (+ b d)]
    (line a b headx heady)
    (line headx heady (+ headx (* -0.086 c) (* -0.05 d)) (+ heady (*  0.05 c) (* -0.086 d)))
    (line headx heady (+ headx (* -0.086 c) (*  0.05 d)) (+ heady (* -0.05 c) (* -0.086 d)))))

;; And we'll usually think of our arrows as having their tails at [0,0]
(defn draw-arrow [[a b]] (draw-offset-arrow [0 0] [a b]))

(defn make-blackboard [title size]
  (create-window title 400 400 white black (- size) size (- size) size)
  (axes)
  (ink yellow))

;; END OF GRAPHICS CODE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; So, with a few bits of graphics code so that the computer can do
;; what Feynman could do on a blackboard:

;; Let us make a blackboard:
(make-blackboard "Arrows!" 10)

;; This arrow points roughly northeast (3 miles east and 4 miles north, in fact)
(def arrow1 [3,4])

;; put it on the blackboard
(draw-arrow arrow1)

;; And this one points more southeast (4 miles east and -3 north, or 3 miles south)
(def arrow2 [4,-3])

;; put it on the blackboard
(draw-arrow arrow2) 

;; And we can add these arrows in what I'd hope is a really obvious way.
(defn add-arrows [arw1 arw2] (mapv + arw1 arw2))

(add-arrows arrow1 arrow2) ;-> [7 1]

;; Draw that too, but in red chalk
(do (ink red) 
    (draw-arrow (add-arrows arrow1 arrow2)))

;; This addition rule looks very simple in terms of coordinates, and geometrically it's 
;; very simple too. 

;; You draw one arrow (it's already there actually)
(do (ink green) (draw-arrow arrow1))

;; And then starting at the head of the first one, you draw the second one
(do (ink green) (draw-offset-arrow arrow1 arrow2))

;; And the sum is the arrow (in red) that points at the head of the
;; second green arrow.

;; It works the same whichever order you do the drawing in.
(make-blackboard "sworrA!" 10)
(draw-arrow arrow1)
(draw-arrow arrow2)
(ink green)
(draw-offset-arrow arrow2 arrow1)
(ink red)
(draw-arrow (add-arrows arrow2 arrow1))

;; It shouldn't be too hard to see how that's going to work for any
;; two arrows to produce a new one. And it should come as no surprise
;; that it works the same way whichever order you add the arrows in.

(add-arrows arrow2 arrow1) ;-> (7 1)
(add-arrows arrow1 arrow2) ;-> (7 1)

;; So the addition of arrows is fairly straightforward, and it gives a
;; way of talking about moving around in the plane. It works exactly
;; as it works for vectors.

;; Now we're going to do something slightly weirder, but in the same spirit.

;; Adding two arrows went like (a,b)+(c,d) -> (a+b, c+d)

;; But we'll say that to multiply two arrows, 

;; (a,b)*(c,d) -> (ac-db, ad+cb)

;; This rule looks a bit odd, but it turns out to be the way to think
;; about composing zooms and twists that I was talking about above,
;; that the ancient philosophers might have come up with if they'd
;; spent their days messing about with photoshop rather than dying of
;; plague or being set on fire by religious people.

;; At any rate it's a very easy rule to explain to a computer:
(defn multiply-arrows[[a b][c d]]
  [(- (* a c) (* d b)) (+ (* a d) (* c b))])

;; And, like arrow-addition, it's also symmetric.
;; Do a few by hand to see how it works!

;; Here's what we get if we multiply our two favourite arrows.
(multiply-arrows arrow1 arrow2) ;-> [24 7]
(multiply-arrows arrow2 arrow1) ;-> [24 7]

;; Here's another way of saying the same thing
(defn multiply-arrows[arrow1 arrow2]
  [(reduce - (map * arrow1 arrow2))
   (reduce + (map * arrow1 (reverse arrow2)))])

(multiply-arrows arrow1 arrow2) ;-> [24 7]
(multiply-arrows arrow2 arrow1) ;-> [24 7]

;; You might be able to see that there's a sense here of multiplying things, 
;; and of exchanging x coordinates and y coordinates, and taking pluses to minuses.

;; You might notice above that arrow2 [4, -3] is arrow1 [3,4] rotated by 90 degrees,
;; and that to get one from the other (3,4) -> (-4, 3), we exchange x and y and
;; swap +4 for -4.

;; In fact, we've encoded the idea of 'rotating and zooming' into our multiplication law.

;; Let's draw our example arrows and what they multiply to

(do 
  (make-blackboard "Arrow1 * Arrow1" 30) ;; <- Much bigger blackboard needed!
  (ink yellow)
  (draw-arrow arrow1)
  (draw-arrow arrow2)
  (ink red)
  (draw-arrow (multiply-arrows arrow1 arrow2)))

;; It turns out that the product of [3,4] and [4,-3] is a gigantic arrow.

(multiply-arrows arrow1 arrow2) ;-> [24 7]


;; What happens, if, instead of taking arrow1 and arrow2 we multiply
;; arrow1 by a much smaller arrow?

;; Arrow 3 will be a tiny arrow, just a bit longer than 1 mile long, and 
;; just a bit to the north of due east.
(def arrow3 [1, 1/10])

(do 
  (make-blackboard "Arrow3 * Arrow1" 10)
  (ink yellow)
  (draw-arrow arrow1)
  (ink cyan)
  (draw-arrow arrow3)
  (ink red)
  (draw-arrow (multiply-arrows arrow3 arrow1)))


;; Arrow 3 represents a very small turn anticlockwise, and a very small zoom

;; Look at what happens if we keep multiplying arrow1 by arrow3

(ink red)
(draw-arrow (multiply-arrows arrow3 arrow1))
(draw-arrow (multiply-arrows arrow3 (multiply-arrows arrow3 arrow1)))
(draw-arrow (multiply-arrows arrow3 (multiply-arrows arrow3 (multiply-arrows arrow3 arrow1))))
;; and so on
(def arrows (iterate (fn[x] (multiply-arrows arrow3 x)) arrow1))
(draw-arrow (nth arrows 3))
(draw-arrow (nth arrows 4))
(draw-arrow (nth arrows 5))
(doseq [ i (take 100 arrows)]
  (draw-arrow i))

;; By repeatedly applying very small turns and very small zooms, we've
;; made a beautiful expanding spiral.


;; And I think, I genuinely think, that you might be able to get small
;; children to play with these arrows and to make this pretty spiral, just
;; after you've taught them about fractions.

















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

