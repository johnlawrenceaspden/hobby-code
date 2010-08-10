;; Some Incanter demo code

;; Basic charts of functions
(use '(incanter core charts latex))
(view (function-plot sin -4 4))
(view (function-plot (fn [x] (* x x)) -4 4))
(view (function-plot (fn [x] (sin (* x x))) -4 4))
(view (function-plot (fn [x] (* 2 x (cos (* x x)))) -4 4))

;; A more complex chart
(doto 
    (function-plot (fn [x] (sin (* x x))) -4 4)
  (add-function (fn [x] (* 2 x (cos (* x x)))) -4 4)
  (add-function (fn [x] (+ (* 2 (cos (* x x)))  (* -1 2 x 2 x (sin (* x x))) )) -2 2)
  (add-latex-subtitle " sin(x^2) \\rightarrow 2xcos(x^2) \\rightarrow 2cos(x^2)-4x^2sin(x^2)")
  view)

;;A chart with controlling sliders
(use '(incanter core stats charts))

(def pdf-chart (function-plot pdf-normal -3 3))
(view pdf-chart)
(add-function pdf-chart pdf-normal -4 4)

(let [x (range -3 3 0.1)] 
  (slider #(set-data pdf-chart [x (pdf-normal x :sd %)]) (range 0.1 10 0.1)))

(let [x (range -3 3 0.1)] 
  (slider #(set-data pdf-chart [x (pdf-normal x :sd %)]) (range 0.1 10 0.1) "sd"))



;;No idea what this is about!
(use '(incanter core charts stats bayes))
(def y [727 583 137])

(def theta (sample-multinomial-params 1000 y))
(def theta1 (sel theta :cols 0))
(def theta2 (sel theta :cols 1))
(def theta3 (sel theta :cols 2))

(mean theta1)
(sd theta1)
(quantile theta1 :probs [0.025 0.975])

(view (histogram theta1 :title "Bush, Sr. Support"))

;;Was the iranian election fair?
(use '(incanter core charts stats io))

(def votes (read-dataset "./data/iran_election_2009.csv" :header true))

(def regions (sel votes :cols :Region))
(def ahmadinejad-votes (sel votes :cols :Ahmadinejad))
(def mousavi-votes (sel votes :cols :Mousavi))
(def rezai-votes (sel votes :cols :Rezai))
(def karrubi-votes (sel votes :cols :Karrubi))

;;Look at the first digits to see if they comply with Benford's law:

(defn first-digit [x]
        (Character/digit (first (str x)) 10))

(def ahmadinejad (map first-digit ahmadinejad-votes))
(def mousavi (map first-digit mousavi-votes))
(def rezai (map first-digit rezai-votes))
(def karrubi (map first-digit karrubi-votes))

;;Calculate Benford probabilities
(defn benford-law [d] (log10 (plus 1 (div d))))
(def benford-probs (benford-law (range 1 11)))
(def benford-freq (mult benford-probs (count regions)))

(defn get-counts [digits]
        (map #(get (:counts (tabulate digits)) % 0)
             (range 1.0 10.0 1.0)))

;;Look at the data!
(doto (xy-plot (range 1 10) benford-freq
                     :legend true :series-label "Predicted"
                     :y-label "First digit frequency"
                     :x-label "First digit"
                     :title "First digit frequency by candidate")
        (add-lines (range 1 10) (get-counts ahmadinejad) :series-label "Ahmadinejad")        
        (add-lines (range 1 10) (get-counts mousavi)     :series-label "Mousavi")
        (add-lines (range 1 10) (get-counts rezai)       :series-label "Rezai")
        (add-lines (range 1 10) (get-counts karrubi)     :series-label "Karrubi")
        
        clear-background
        view)


;;Now chi-squared test to check whether the counts could have come from 
;;a Benford's law-satisfying distribution
(def ahmadinejad-test
           (chisq-test :table (get-counts ahmadinejad)
                       :probs benford-probs))

(:X-sq ahmadinejad-test)
(:p-value ahmadinejad-test)

(def mousavi-test
           (chisq-test :table (get-counts mousavi)
                       :probs benford-probs))

(:X-sq mousavi-test)
(:p-value mousavi-test)

;; Irises
(use '(incanter core charts stats datasets))

(view (scatter-plot :Sepal.Length :Sepal.Width :data (get-dataset :iris)))

(doto
    (scatter-plot :Sepal.Length :Sepal.Width :data (get-dataset :iris))
  (set-stroke-color java.awt.Color/gray)
  view)

(view (scatter-plot :Sepal.Length :Sepal.Width :group-by :Species :data (get-dataset :iris)))

;; Dynamic xy plot
(let [x (range -3 3 0.1)]
	  (view (dynamic-xy-plot [mean (range -3 3 0.1)
	                          std-dev (range 0.1 10 0.1)]
	          [x (pdf-normal x :mean mean :sd std-dev)])))