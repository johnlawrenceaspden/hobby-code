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

;; Before we talk about the complex numbers, let's think about Feynman's arrows.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GRAPHICS CODE

;; I'd like to draw some pictures.  And I'd like
;; to use my 'simple-plotter' library, which is on clojars
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
;; what Feynman could do on a blackboard.

;; Let us make a blackboard:
(make-blackboard "Arrows!" 10)

;; This arrow points roughly northeast (3 inches east and 4 inches north, in fact)
(def arrow1 [3,4])

;; put it on the blackboard
(draw-arrow arrow1)

;; And this one points more southeast (4 inches east and -3 north, or 3 south)
(def arrow2 [4,-3])

;; put it on the blackboard
(draw-arrow arrow2) 

;; And we can add these arrows in what I'd hope is a really obvious way.
(defn add-arrows [arw1 arw2] (mapv + arw1 arw2))

(add-arrows arrow1 arrow2) ;-> [7 1]

;; Draw that too, but in red chalk
(ink red) 
(draw-arrow (add-arrows arrow1 arrow2))

;; This addition rule looks very simple in terms of coordinates, and geometrically it's 
;; very simple too. 

;; You draw one arrow
(ink green)
(draw-arrow arrow1)

;; And then starting at the head of the first one, you draw the second one

;; here it is
(draw-offset-arrow arrow1 arrow2)

;; And the sum is the arrow (in red) that points at the head of the second arrow.

;; It works the same whichever order you do the drawing in!
(make-blackboard "Commute!" 10)
(draw-arrow arrow2)
(ink green)
(draw-offset-arrow arrow2 arrow1)
(ink red)
(draw-arrow (add-arrows arrow2 arrow1))

;; It shouldn't be too hard to see how that's going to work for any
;; two arrows to produce a new one And it should come as no surprise
;; that it works the same way whichever order you add the arrows in.

(add-arrows arrow2 arrow1) ;-> (7 1)
(add-arrows arrow1 arrow2) ;-> (7 1)

;; So the addition of arrows is fairly straightforward, and it gives a way of talking about
;; moving around in the plane. It works exactly as it works for vectors.

;; But we want to do better than vectors. We want to be able to talk
;; about zooming in and turning in photoshop, and possibly in other
;; contexts as well.

;; So we're going to make a multiplication We could also try multiplying them, and for this it turns out that a good thing to take is
;; (a,b)*(c,d)=(ac-db, ad+cb)

;; This rule looks a bit odd. Stick with it. It turns out to be the thing about photoshop
;; that I was talking about above.

;; At any rate it's a very easy rule to explain to a computer!
(defn multiply-arrows[[a b][c d]]
  [(- (* a c) (* d b)) (+ (* a d) (* c b))])

;; And it's also symmetric. Do a few by hand to see how it works!
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

;; Let's draw some examples

(do 
  (cls)
  (ink white)
  (axes)
  (ink yellow)
  (draw-arrow arrow1)
  (draw-arrow arrow2)
  (ink red)
  (draw-arrow (multiply-arrows arrow1 arrow2)))

;; Oops, we've shot off the screen! 

(multiply-arrows arrow1 arrow2) ;-> [24 7]

;; How long is that thing anyway? Remember Pythagoras?
(defn length [arrow]
  (Math/sqrt (reduce + (map * arrow arrow))))

(length (multiply-arrows arrow1 arrow2)) ;-> 25.0

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

;; It represents a very small turn, and a very small zoom

;; Look at what happens if we keep multiplying arrow2 by arrow3

(iterate (fn[x] (multiply-arrows arrow3 x)) arrow1) 
;-> ([3 4] [2.6 4.3] [2.17 4.56] [1.714 4.776999999999999] [1.2363 4.9483999999999995] [0.74146 5.07203] [0.23425700000000005 5.146176] [-0.28036059999999996 5.169601699999999] [-0.7973207699999999 5.14156564] [-1.3114773339999999 5.0618335629999995] [-1.8176606902999999 4.9306858296] [-2.3107292732599998 4.74891976057] [-2.785621249317 4.517846833244] [-3.2374059326414 4.2392847083123] [-3.66133440347263 3.91554411504816] [-4.052888814977446 3.549410674700897] [-4.407829882447535 3.144121793203152] [-4.72224206176785 2.7033388049583986] [-4.99257594226369 2.2311145987816134] [-5.215687402141851 1.7318570045552444] [-5.388873102597376 1.210288264341059] [-5.509901929031482 0.6714009540813214] [-5.577042024439614 0.12041076117817318] [-5.589083100557431 -0.4372934412657883] [-5.545353756430853 -0.9962017513215314] [-5.4457335812986996 -1.5507371269646169] [-5.290659868602238 -2.095310485094487] ...)


(draw-arrow arrow1)
(ink red)
(draw-arrow (multiply-arrows arrow3 arrow1))
(draw-arrow (multiply-arrows arrow3 (multiply-arrows arrow3 arrow1)))
(def arrows (iterate (fn[x] (multiply-arrows arrow3 x)) arrow1))
(draw-arrow (nth arrows 3))
(draw-arrow (nth arrows 4))
(draw-arrow (nth arrows 5))
(doseq [ i (take 100 arrows)]
  (draw-arrow i))

;; By repeatedly applying very small turns and very small zooms, we've
;; made a beautiful expanding spiral.

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

