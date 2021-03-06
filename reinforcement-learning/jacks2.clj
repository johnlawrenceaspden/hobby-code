;; Jack's Car Rental from the reinforcement learning book

;; The maximal number of cars at each site
(def max-cars 20)
;; Jack is something of a short-termist
(def gamma 0.9)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Useful utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Absolute Value
(defn abs[x] (if (< x 0) (- x) x))
(map abs [-2 0 1 2]) ; (2 0 1 2) ; 

;; Pretty-printing truncation
(defn twosf [x] (if (and (number? x) (not (integer? x))) (float (/ (Math/round (* x 100.0)) 100)) x))

;; Print the first twenty of anything to two s.f.
(use '[clojure.walk :only (postwalk)])
(defn ps ([s]    (ps 20 s))
         ([n s] (postwalk twosf (take n s))))
(ps [22/7 [2.345 [23.456 #{ Math/PI {:a 2}}]]] ) ; [3.14 [2.35 [23.46 #{3.14 {:a 2}}]]]

;; Inclusive range 
(defn irange[n] (range (inc n)))
(irange 3) ; (0 1 2 3)

;; define a memoized function, ripped from the old clojure.contrib
(defmacro defn-memo
  "Just like defn, but memoizes the function using clojure.core/memoize"
  [fn-name & defn-stuff]
  `(do
     (defn ~fn-name ~@defn-stuff)
     (alter-var-root (var ~fn-name) memoize)
     (var ~fn-name)))

;; Calculate the values, total probability and expectation of a probability function on the natural numbers (range)
(defn fair-die [n] (fn [s] (if (<= 1 s n) (/ 1. n) 0)))

(defn vseq [f] (map f (range))) 
(defn pseq [f] (reductions + (map f (range)))) ; should tend to one
(defn eseq [f] (reductions + (map * (range) (map f (range))))) ; should tend to the expectation of the distribution

(ps (vseq (fair-die 8))) ; (0 0.13 0.13 0.13 0.13 0.13 0.13 0.13 0.13 0 0 0 0 0 0 0 0 0 0 0)
(ps (pseq (fair-die 8))) ; (0 0.13 0.25 0.38 0.5 0.63 0.75 0.88 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0)
(ps (eseq (fair-die 8))) ; (0 0.13 0.38 0.75 1.25 1.88 2.63 3.5 4.5 4.5 4.5 4.5 4.5 4.5 4.5 4.5 4.5 4.5 4.5 4.5)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Defining the Poisson distribution, and a capped variant for e.g. poiss(l)>=5
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn-memo factorial [n] (assert (and (integer? n) (>= n 0))) (reduce * (take n (iterate inc 1N)))) ; #'user/factorial

(map factorial (range)) ; (1 1N 2N 6N 24N 120N 720N 5040N 40320N 362880N 3628800N 39916800N 479001600N 6227020800N 87178291200N 1307674368000N 20922789888000N 355687428096000N 6402373705728000N 121645100408832000N 2432902008176640000N 51090942171709440000N 1124000727777607680000N 25852016738884976640000N 620448401733239439360000N 15511210043330985984000000N 403291461126605635584000000N 10888869450418352160768000000N 304888344611713860501504000000N 8841761993739701954543616000000N 265252859812191058636308480000000N 8222838654177922817725562880000000N 263130836933693530167218012160000000N 8683317618811886495518194401280000000N 295232799039604140847618609643520000000N 10333147966386144929666651337523200000000N 371993326789901217467999448150835200000000N 13763753091226345046315979581580902400000000N 523022617466601111760007224100074291200000000N 20397882081197443358640281739902897356800000000N 815915283247897734345611269596115894272000000000N 33452526613163807108170062053440751665152000000000N 1405006117752879898543142606244511569936384000000000N 60415263063373835637355132068513997507264512000000000N 2658271574788448768043625811014615890319638528000000000N 119622220865480194561963161495657715064383733760000000000N 5502622159812088949850305428800254892961651752960000000000N 258623241511168180642964355153611979969197632389120000000000N 12413915592536072670862289047373375038521486354677760000000000N 608281864034267560872252163321295376887552831379210240000000000N 30414093201713378043612608166064768844377641568960512000000000000N 1551118753287382280224243016469303211063259720016986112000000000000N 80658175170943878571660636856403766975289505440883277824000000000000N 4274883284060025564298013753389399649690343788366813724672000000000000N 230843697339241380472092742683027581083278564571807941132288000000000000N 12696403353658275925965100847566516959580321051449436762275840000000000000N 710998587804863451854045647463724949736497978881168458687447040000000000000N 40526919504877216755680601905432322134980384796226602145184481280000000000000N 2350561331282878571829474910515074683828862318181142924420699914240000000000000N 138683118545689835737939019720389406345902876772687432540821294940160000000000000N 8320987112741390144276341183223364380754172606361245952449277696409600000000000000N 507580213877224798800856812176625227226004528988036003099405939480985600000000000000N 31469973260387937525653122354950764088012280797258232192163168247821107200000000000000N 1982608315404440064116146708361898137544773690227268628106279599612729753600000000000000N 126886932185884164103433389335161480802865516174545192198801894375214704230400000000000000N 8247650592082470666723170306785496252186258551345437492922123134388955774976000000000000000N 544344939077443064003729240247842752644293064388798874532860126869671081148416000000000000000N 36471110918188685288249859096605464427167635314049524593701628500267962436943872000000000000000N 2480035542436830599600990418569171581047399201355367672371710738018221445712183296000000000000000N 171122452428141311372468338881272839092270544893520369393648040923257279754140647424000000000000000N 11978571669969891796072783721689098736458938142546425857555362864628009582789845319680000000000000000N 850478588567862317521167644239926010288584608120796235886430763388588680378079017697280000000000000000N 61234458376886086861524070385274672740778091784697328983823014963978384987221689274204160000000000000000N 4470115461512684340891257138125051110076800700282905015819080092370422104067183317016903680000000000000000N 330788544151938641225953028221253782145683251820934971170611926835411235700971565459250872320000000000000000N 24809140811395398091946477116594033660926243886570122837795894512655842677572867409443815424000000000000000000N 1885494701666050254987932260861146558230394535379329335672487982961844043495537923117729972224000000000000000000N 145183092028285869634070784086308284983740379224208358846781574688061991349156420080065207861248000000000000000000N 11324281178206297831457521158732046228731749579488251990048962825668835325234200766245086213177344000000000000000000N 894618213078297528685144171539831652069808216779571907213868063227837990693501860533361810841010176000000000000000000N 71569457046263802294811533723186532165584657342365752577109445058227039255480148842668944867280814080000000000000000000N 5797126020747367985879734231578109105412357244731625958745865049716390179693892056256184534249745940480000000000000000000N 475364333701284174842138206989404946643813294067993328617160934076743994734899148613007131808479167119360000000000000000000N 39455239697206586511897471180120610571436503407643446275224357528369751562996629334879591940103770870906880000000000000000000N 3314240134565353266999387579130131288000666286242049487118846032383059131291716864129885722968716753156177920000000000000000000N 281710411438055027694947944226061159480056634330574206405101912752560026159795933451040286452340924018275123200000000000000000000N 24227095383672732381765523203441259715284870552429381750838764496720162249742450276789464634901319465571660595200000000000000000000N 2107757298379527717213600518699389595229783738061356212322972511214654115727593174080683423236414793504734471782400000000000000000000N 185482642257398439114796845645546284380220968949399346684421580986889562184028199319100141244804501828416633516851200000000000000000000N 16507955160908461081216919262453619309839666236496541854913520707833171034378509739399912570787600662729080382999756800000000000000000000N 1485715964481761497309522733620825737885569961284688766942216863704985393094065876545992131370884059645617234469978112000000000000000000000N 135200152767840296255166568759495142147586866476906677791741734597153670771559994765685283954750449427751168336768008192000000000000000000000N 12438414054641307255475324325873553077577991715875414356840239582938137710983519518443046123837041347353107486982656753664000000000000000000000N 1156772507081641574759205162306240436214753229576413535186142281213246807121467315215203289516844845303838996289387078090752000000000000000000000N 108736615665674308027365285256786601004186803580182872307497374434045199869417927630229109214583415458560865651202385340530688000000000000000000000N 10329978488239059262599702099394727095397746340117372869212250571234293987594703124871765375385424468563282236864226607350415360000000000000000000000N 991677934870949689209571401541893801158183648651267795444376054838492222809091499987689476037000748982075094738965754305639874560000000000000000000000N 96192759682482119853328425949563698712343813919172976158104477319333745612481875498805879175589072651261284189679678167647067832320000000000000000000000N 9426890448883247745626185743057242473809693764078951663494238777294707070023223798882976159207729119823605850588608460429412647567360000000000000000000000N 933262154439441526816992388562667004907159682643816214685929638952175999932299156089414639761565182862536979208272237582511852109168640000000000000000000000N ...)

(defn-memo poisson [lambda, n]
  (assert (and (integer? n) (>= n 0)))
  (* (Math/exp (- lambda))
     (/ (Math/pow lambda n)(factorial n) )))



(ps (vseq (partial poisson 2))) ; (0.14 0.27 0.27 0.18 0.09 0.04 0.01 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0) 
(ps (vseq (partial poisson 3))) ; (0.05 0.15 0.22 0.22 0.17 0.1 0.05 0.02 0.01 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0)
(ps (vseq (partial poisson 4))) ; (0.02 0.07 0.15 0.2 0.2 0.16 0.1 0.06 0.03 0.01 0.01 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0)

(ps (pseq (partial poisson 2))) ; (0.14 0.41 0.68 0.86 0.95 0.98 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0) 
(ps (pseq (partial poisson 3))) ; (0.05 0.2 0.42 0.65 0.82 0.92 0.97 0.99 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0) 
(ps (pseq (partial poisson 4))) ; (0.02 0.09 0.24 0.43 0.63 0.79 0.89 0.95 0.98 0.99 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0)

(ps (eseq (partial poisson 2))) ; (0.0 0.27 0.81 1.35 1.71 1.89 1.97 1.99 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0) 
(ps (eseq (partial poisson 3))) ; (0.0 0.15 0.6 1.27 1.94 2.45 2.75 2.9 2.96 2.99 3.0 3.0 3.0 3.0 3.0 3.0 3.0 3.0 3.0 3.0) 
(ps (eseq (partial poisson 4))) ; (0.0 0.07 0.37 0.95 1.73 2.52 3.14 3.56 3.8 3.91 3.97 3.99 4.0 4.0 4.0 4.0 4.0 4.0 4.0 4.0)

;; And for this example we're truncating the poisson distribution, so we might be interested in, say, the chance of poisson 3 producing 6 or above.

(defn-memo poisson>= [lambda, n]
  (- 1.0 (reduce + (map (partial poisson lambda) (range n)))))


(defn poiss-test [lambda n]
  (let [l      (for [i (range n)] (poisson lambda i))
        g      (poisson>= lambda n)]
      [lambda n :| l g :| (+ g (reduce + l))])) ; #'user/poiss-test

(ps (poiss-test 3 7)) ; (3 7 :| (0.05 0.15 0.22 0.22 0.17 0.1 0.05) 0.03 :| 1.0) 
(ps (poiss-test 5 5)) ; (5 5 :| (0.01 0.03 0.08 0.14 0.18) 0.56 :| 1.0)
(ps (poiss-test 5 1)) ; (5 1 :| (0.01) 0.99 :| 1.0) 

(defn-memo capped-poisson [lambda, cap, n]
  (assert (> lambda 0 ))
  (assert (>= n 0))
  (assert (>= cap 0))
  (cond (< n cap) (poisson lambda n)
        (= n cap) (poisson>= lambda cap)
        :else 0))

(ps (vseq (partial capped-poisson 2 5))) ; (0.14 0.27 0.27 0.18 0.09 0.05 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
(ps (vseq (partial capped-poisson 3 5))) ; (0.05 0.15 0.22 0.22 0.17 0.18 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
(ps (vseq (partial capped-poisson 4 5))) ; (0.02 0.07 0.15 0.2 0.2 0.37 0 0 0 0 0 0 0 0 0 0 0 0 0 0) 

(ps (pseq (partial capped-poisson 2 5))) ; (0.14 0.41 0.68 0.86 0.95 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0)
(ps (pseq (partial capped-poisson 3 5))) ; (0.05 0.2 0.42 0.65 0.82 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0) 
(ps (pseq (partial capped-poisson 4 5))) ; (0.02 0.09 0.24 0.43 0.63 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0) 

(ps (eseq (partial capped-poisson 2 5))) ; (0.0 0.27 0.81 1.35 1.71 1.98 1.98 1.98 1.98 1.98 1.98 1.98 1.98 1.98 1.98 1.98 1.98 1.98 1.98 1.98)
(ps (eseq (partial capped-poisson 3 5))) ; (0.0 0.15 0.6 1.27 1.94 2.87 2.87 2.87 2.87 2.87 2.87 2.87 2.87 2.87 2.87 2.87 2.87 2.87 2.87 2.87) 
(ps (eseq (partial capped-poisson 4 5))) ; (0.0 0.07 0.37 0.95 1.73 3.59 3.59 3.59 3.59 3.59 3.59 3.59 3.59 3.59 3.59 3.59 3.59 3.59 3.59 3.59)

(defn-memo capped-poisson-expectation [lambda cap]
  (reduce + (map *
                 (range)
                 (map (partial capped-poisson lambda cap) (range (inc cap))))))

(ps (for [i (range)] (capped-poisson-expectation 3 i))) ; (0.0 0.95 1.75 2.33 2.68 2.87 2.95 2.98 2.99 3.0 3.0 3.0 3.0 3.0 3.0 3.0 3.0 3.0 3.0 3.0)
(ps (for [i (range)] (capped-poisson-expectation 10 i))) ; (0.0 1.0 2.0 3.0 3.99 4.96 5.89 6.76 7.54 8.21 8.75 9.17 9.47 9.68 9.81 9.9 9.95 9.97 9.99 9.99)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A day in the life of Jack's:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; One evening, Jack's has 7 cars at location one
;; and 2 cars at location two
[7,2] ; [7 2]

;; Jack decides to move 2 cars (from one to two), paying the transfer cost of
(* (abs 2) 2) ; $4

;; Jack's action is valid
(> 7 2) ; true

;; afterwards, the car count is:
[(- 7 2),(+ 2 2)] ; [5 4]

;; The following morning, 
;; at location one there are
5
;; cars
;; we can therefore successfully accomodate up to 5 rental requests, the rest are wasted
(rand-int 6) ; 4
;; We receive 4 rental requests with probability
(capped-poisson 3 5 4) ; 0.16803135574154082
;; making
(* 4 10) ; 40
;; $40 profit
;; and location 1 then has
(- 5 4) ; 1
;; cars

;; at location one
;; we now have 0 cars,
;; the number of returns accepted can therefore range from 0 to 20 (extra cars disappear by magic)
;; we get
(rand-int 20) ; 14
;; with probability
(capped-poisson 3 20 14) ; 2.73152870200298E-6
;; and therefore have 
(+ 0 14) ; 14
;; in total
;; at the end of the day


;; at location 2 we start with
4
;; cars
;; can therefore receive up to four requests
(rand-int 5) ; 2
;; requests
;; 2 cars are in fact rented, which happens with probability
(capped-poisson 4 4 2) ; 0.14652511110987343
;; location 2 then has
(- 4 4) ; 0
;; cars
;; and we make
(* 4 10) ; 40
;; $40 profit

;; we can accept up to
(- 20 0) ; 20
;; returns
(rand-int 20) ; 0
;; with probability 
(capped-poisson 2 20 0) ; 0.1353352832366127

;; and we end the day with
[(+ 0 14) (+ 0 0)] ; [14 0]
;; cars
;; a reward of 
(+ (* 4 10) (* 4 10) (* (abs 2) -2)) ; $76
;; for the day

;; cars often get picked up from two but returned to one
;; lambdas are
;; location one hire 3
(def one-hire 3)
;; location two hire 4
(def two-hire 4)
;; location one return 3
(def one-return 3)
;; location two return 2
(def two-return 2)

;; state [7,2] , action 2, cost $4, state [5,4], rentals [4,2], payout $60, state [1,2], returns [14,0], state [15,2]
;; the total probability of the day was:
(* (capped-poisson one-hire 5 4) (capped-poisson two-hire 4 2) (capped-poisson one-return 20 14) (capped-poisson two-return 20 0)) ; 9.101630393225908E-9
;; But note that the chain transition
;; state [7,2] , action 2 -> payout $56, state [15,2]
;; can happen in several ways, can choose any a,b : a+b=6, and have [a,b] rentals and [15-(7-2)+a, 2-(2+2)+b] returns and will get the same transition


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Estimate the value function for the policy 'never move any cars'
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn poneadowntob [a b] (assert (<= 0 b a max-cars)) (capped-poisson one-hire a (- a b)))
(defn ptwoadowntob [a b] (assert (<= 0 b a max-cars)) (capped-poisson two-hire a (- a b)))
(defn ponebuptoc   [b c] (assert (<= 0 b c max-cars)) (capped-poisson one-return (- max-cars b) (- c b)))
(defn ptwobuptoc   [b c] (assert (<= 0 b c max-cars)) (capped-poisson two-return (- max-cars b) (- c b)))

(defn pathsoneatoc [a c]
  (for [b (range (inc (min a c)))]
    (* (poneadowntob a b) (ponebuptoc b c))))

(defn pathstwoatoc [a c]
  (for [b (range (inc (min a c)))]
    (* (ptwoadowntob a b) (ptwobuptoc b c))))

(def poneatoc (memoize (fn [a c] (reduce + (pathsoneatoc a c)))))
(def ptwoatoc (memoize (fn [a c] (reduce + (pathstwoatoc a c)))))

(def car-range (range (inc max-cars)))
(def states (for [i car-range j car-range] [i,j]))

(ps (partition (inc max-cars) (for [[i j] states] (poneatoc i j))))
(ps (partition (inc max-cars) (for [[i j] states] (ptwoatoc i j))))


(def ptransition (memoize (fn [[a,b] [c,d] action]
  (assert (and (>= a action) (<= (+ b action) max-cars)))
  (let [[a,b] [(- a action) (+ b action)]]
    (* (poneatoc a c)
       (ptwoatoc b d))))))

(rand-nth (for [[a b] states [c d] states]
  (let [maxaction (min a (- max-cars b))
        minaction (- (min b (- max-cars a)))]
    (for [action (range minaction (inc maxaction))]
      [[a b][c d] action (ptransition [a b] [c d] action)]))))


(defn-memo expected-reward-from-state [[a,b]]
  (* 10 (+ (capped-poisson-expectation one-hire a)
           (capped-poisson-expectation two-hire b))))

(defn-memo expected-reward [[a,b] action]
  (assert (and (>= a action) (<= (+ b action) max-cars)))
  (let [[a,b] [(- a action) (+ b action)]]
    (+ (* (abs action) -2) (expected-reward-from-state [a,b]))))


(defn update-val [v [m,n] action]
  (+ (expected-reward [m,n] action)
     (* gamma
        (reduce + (for [s states] (* (ptransition [m,n] s action) (v s)))))))


(def vzero (into {} (for [i car-range j car-range] [[i,j] 0])))

(update-val vzero [0,0] 0)
(update-val vzero [1,0] 0)

(update-val vzero [0,1] 0) ; 9.50212931632136

(expected-reward [0,1] 0) ; 9.816843611112658 ; 9.50212931632136 ; 9.50212931632136 ; 9.50212931632136
(capped-poisson 4 1 1) ; 0.9816843611112658 ; 0.9816843611112658 ; 0.9816843611112658
(expected-reward-from-state [0,1]) ; 9.816843611112658 ; 9.50212931632136






(def vone (into {} (for [i car-range j car-range] [[i,j] (update-val vzero [i,j] 0)])))

(def vtwo (into {} (for [i car-range j car-range] [[i,j] (update-val vone [i,j] 0)])))



(ps 1000 (sort vone)) ; ([[0 0] 0.0] [[0 1] 9.82] [[0 2] 18.9] [[0 3] 26.52] [[0 4] 32.19] [[0 5] 35.9] [[1 0] 9.5] [[1 1] 19.32] [[1 2] 28.4] [[1 3] 36.02] [[1 4] 41.69] [[1 5] 45.4] [[2 0] 17.51] [[2 1] 27.33] [[2 2] 36.41] [[2 3] 44.03] [[2 4] 49.7] [[2 5] 53.41] [[3 0] 23.28] [[3 1] 33.1] [[3 2] 42.18] [[3 3] 49.8] [[3 4] 55.46] [[3 5] 59.18] [[4 0] 26.81] [[4 1] 36.62] [[4 2] 45.71] [[4 3] 53.33] [[4 4] 58.99] [[4 5] 62.7] [[5 0] 28.65] [[5 1] 38.47] [[5 2] 47.55] [[5 3] 55.17] [[5 4] 60.84] [[5 5] 64.55]) ; ([[0 0] 0.0] [[0 1] 9.82] [[0 2] 18.9] [[0 3] 26.52] [[0 4] 32.19] [[0 5] 35.9] [[1 0] 9.5] [[1 1] 19.32] [[1 2] 28.4] [[1 3] 36.02] [[1 4] 41.69] [[1 5] 45.4] [[2 0] 17.51] [[2 1] 27.33] [[2 2] 36.41] [[2 3] 44.03] [[2 4] 49.7] [[2 5] 53.41] [[3 0] 23.28] [[3 1] 33.1] [[3 2] 42.18] [[3 3] 49.8] [[3 4] 55.46] [[3 5] 59.18] [[4 0] 26.81] [[4 1] 36.62] [[4 2] 45.71] [[4 3] 53.33] [[4 4] 58.99] [[4 5] 62.7] [[5 0] 28.65] [[5 1] 38.47] [[5 2] 47.55] [[5 3] 55.17] [[5 4] 60.84] [[5 5] 64.55])

(ps 1000 (sort vtwo)) ; ([[0 0] 33.94] [[0 1] 43.87] [[0 2] 53.52] [[0 3] 62.51] [[0 4] 70.43] [[0 5] 76.98] [[1 0] 43.62] [[1 1] 53.56] [[1 2] 63.21] [[1 3] 72.2] [[1 4] 80.12] [[1 5] 86.66] [[2 0] 52.29] [[2 1] 62.22] [[2 2] 71.87] [[2 3] 80.86] [[2 4] 88.78] [[2 5] 95.32] [[3 0] 59.26] [[3 1] 69.2] [[3 2] 78.85] [[3 3] 87.84] [[3 4] 95.76] [[3 5] 102.3] [[4 0] 64.29] [[4 1] 74.22] [[4 2] 83.87] [[4 3] 92.87] [[4 4] 100.79] [[4 5] 107.33] [[5 0] 67.56] [[5 1] 77.5] [[5 2] 87.15] [[5 3] 96.14] [[5 4] 104.06] [[5 5] 110.6])


(println "time for longest update operation")
(time (update-val vzero [max-cars,max-cars] 0)) ; 64.55075249295302 ; 64.55075249295302 ; 64.55075249295302


(defn jacobi [v] (into {} (for [s states] [s (update-val v s 0)])))

(jacobi vzero)

(def inplace (fn [v [[i,j] action]] (assoc v [i,j] (update-val v [i,j] action))))

(update-val vzero [1,1] 0) ; 19.318972927434018
( (inplace vzero [[1,1] 0]) [1,1]) ; 19.318972927434018

(defn gauss-seidel [v] (reduce inplace v (for [s states][s 0])))

(gauss-seidel vzero)

(defn over-relax [v [i,j] action omega]
  (let [a (v [i,j])
        d (- (update-val v [i,j] action)  a)]
    (+ a (* omega d))))

(over-relax vzero [1,1] 0 2.0) ; 38.637945854868036 
(over-relax vzero [max-cars,max-cars] 0 2.0) ; 139.99999995290918

(defn sor-inplace[omega]
  (fn [v [[i,j] action]] (assoc v [i,j] (over-relax v [i,j] action omega))))

(print "max-cars " max-cars "time for one iteration of SOR")
(time (reduce (sor-inplace 2.0) vzero (for [s states][s 0]))) 

;; timings with direct calculation gives 50x speedup and n^5 turns into n^3.5

;; max-cars  20 time for one iteration of SOR"Elapsed time: 580.522499 msecs"
;; max-cars  15 time for one iteration of SOR"Elapsed time: 186.503352 msecs"
;; max-cars  12 time for one iteration of SOR"Elapsed time: 82.125655 msecs"
;; max-cars  11 time for one iteration of SOR"Elapsed time: 57.395305 msecs"
;; max-cars  10 time for one iteration of SOR"Elapsed time: 42.425916 msecs"
;; max-cars  6 time for one iteration of SOR"Elapsed time: 8.944654 msecs"
;; max-cars  5 time for one iteration of SOR"Elapsed time: 5.03039 msecs"

(ps (map (fn[x] (Math/log x)) [20  15  12 11 10 6 5])) ; (3.0 2.71 2.48 2.4 2.3 1.79 1.61)
(ps (map (fn[x] (Math/log x)) [580 186 82 57 42 9 5]))              ; (6.36 5.23 4.41 4.04 3.74 2.2 1.61)

(/ (- 6.36 1.61) (- 3.0 1.61)) ; 3.41726618705036


;; timings after cutting down the intermediate steps

;; max-cars  20 time for one iteration of SOR"Elapsed time: 81160.131641 msecs"
;; max-cars  15 time for one iteration of SOR"Elapsed time: 16723.008293 msecs"
;; max-cars  12 time for one iteration of SOR"Elapsed time: 5072.422096 msecs"
;; max-cars  11 time for one iteration of SOR"Elapsed time: 3244.704042 msecs"
;; max-cars  10 time for one iteration of SOR"Elapsed time: 1954.167973 msecs"
;; max-cars  6 time for one iteration of SOR"Elapsed time: 161.526508 msecs"
;; max-cars  5 time for one iteration of SOR"Elapsed time: 70.51998 msecs"

;; timings pre-when statement

;; max-cars 20 time for one iteration of SOR"Elapsed time: 155415.417142 msecs"
;; max-cars 15 time for one iteration of SOR"Elapsed time: 32325.9033 msecs"
;; max cars 12 time for one iteration of SOR"Elapsed time: 9054.616065 msecs"
;; max-cars 11 time for one iteration of SOR"Elapsed time: 6523.300923 msecs"
;; max-cars 10 time for one iteration of SOR"Elapsed time: 4085.586055 msecs"
;; max-cars 6  time for one iteration of SOR"Elapsed time: 258.344699 msecs"
;; max-cars 5  time for one iteration of SOR"Elapsed time: 130.422911 msecs"

(ps (map (fn[x] (Math/log x)) [20     15    12   11   10   6   5]))   ; (3.0    2.71 2.48  2.4  2.3 1.79 1.61)
(ps (map (fn[x] (Math/log x)) [155415 32325 9054 6523 4085 258 130])) ; (11.95 10.38 9.11 8.78 8.32 5.55 4.87)

(/ (- 11.95 4.87) (- 3.0 1.61)) ; 5.0935251798561145
                                     
;; Oh God I wrote an n^5 algorithm, actually why isn't it n^6 just to calculate the matrix?





(defn sor[omega] (fn [v] (println ".") (reduce (sor-inplace omega) v (for [s states][s 0]))))


(def vseries-sor  (iterate (sor 1.5) vzero))

(ps 20 (for [v vseries-sor] (v [3,3]))) ; (0 111.65 229.16 322.13 379.46 415.27 436.64 449.97 457.66 461.34 462.76 463.38 463.7 463.83 463.89 463.92 463.94 463.94 463.95 463.95)

(def final-sor (nth vseries-sor 20))

(def testm (jacobi final-sor))
(def lastmove (apply max (for [s states] (- (testm s) (final-sor s)))))

lastmove ; 0.0015506759456229702

(println "a final jacobi update gives |delta|=: " lastmove)

(defn pvalue [vfn]
  (doseq [i (reverse (partition (inc max-cars)  (sort vfn)))] (clojure.pprint/cl-format true "~{~$ ~}~%" (map (fn[[[a,b] x]] x) i))))

(defn ppolicy [policy]
  (doseq [i (reverse (partition (inc max-cars)  (sort policy)))] (clojure.pprint/cl-format true "~{~2D ~}~%" (map (fn[[[a,b] x]] x) i))))


(pvalue final-sor)

;; ;; five by five case
;; [[0 0] 382.71] [[0 1] 392.67] [[0 2] 402.46] [[0 3] 411.83] [[0 4] 420.46] [[0 5] 428.01]
;; [[1 0] 392.46] [[1 1] 402.42] [[1 2] 412.2]  [[1 3] 421.57] [[1 4] 430.2]  [[1 5] 437.76]
;; [[2 0] 401.36] [[2 1] 411.32] [[2 2] 421.11] [[2 3] 430.48] [[2 4] 439.11] [[2 5] 446.66]
;; [[3 0] 408.82] [[3 1] 418.78] [[3 2] 428.57] [[3 3] 437.94] [[3 4] 446.57] [[3 5] 454.12]
;; [[4 0] 414.52] [[4 1] 424.48] [[4 2] 434.27] [[4 3] 443.63] [[4 4] 452.26] [[4 5] 459.82]
;; [[5 0] 418.49] [[5 1] 428.45] [[5 2] 438.24] [[5 3] 447.61] [[5 4] 456.24] [[5 5] 463.79]

;; ;; ten by ten case
;; [[0 0] 404.5]  [[0 1] 414.47] [[0 2] 424.33] [[0 3] 433.96] [[0 4] 443.23] [[0 5] 452.07] [[0 6] 460.44] [[0 7] 468.31] [[0 8] 475.63] [[0 9] 482.27] [[0 10] 488.05]
;; [[1 0] 414.31] [[1 1] 424.28] [[1 2] 434.14] [[1 3] 443.77] [[1 4] 453.04] [[1 5] 461.88] [[1 6] 470.25] [[1 7] 478.12] [[1 8] 485.44] [[1 9] 492.08] [[1 10] 497.86]
;; [[2 0] 423.5]  [[2 1] 433.47] [[2 2] 443.34] [[2 3] 452.96] [[2 4] 462.23] [[2 5] 471.07] [[2 6] 479.44] [[2 7] 487.31] [[2 8] 494.63] [[2 9] 501.27] [[2 10] 507.05]
;; [[3 0] 431.64] [[3 1] 441.61] [[3 2] 451.48] [[3 3] 461.1]  [[3 4] 470.37] [[3 5] 479.21] [[3 6] 487.58] [[3 7] 495.45] [[3 8] 502.77] [[3 9] 509.41] [[3 10] 515.19]
;; [[4 0] 438.51] [[4 1] 448.48] [[4 2] 458.35] [[4 3] 467.97] [[4 4] 477.24] [[4 5] 486.08] [[4 6] 494.45] [[4 7] 502.32] [[4 8] 509.64] [[4 9] 516.28] [[4 10] 522.06]
;; [[5 0] 444.13] [[5 1] 454.1]  [[5 2] 463.96] [[5 3] 473.59] [[5 4] 482.86] [[5 5] 491.7]  [[5 6] 500.07] [[5 7] 507.94] [[5 8] 515.26] [[5 9] 521.9]  [[5 10] 527.68]
;; [[6 0] 448.62] [[6 1] 458.59] [[6 2] 468.45] [[6 3] 478.08] [[6 4] 487.35] [[6 5] 496.19] [[6 6] 504.56] [[6 7] 512.43] [[6 8] 519.75] [[6 9] 526.39] [[6 10] 532.17]
;; [[7 0] 452.13] [[7 1] 462.11] [[7 2] 471.97] [[7 3] 481.6]  [[7 4] 490.87] [[7 5] 499.7]  [[7 6] 508.07] [[7 7] 515.95] [[7 8] 523.26] [[7 9] 529.9]  [[7 10] 535.69]
;; [[8 0] 454.81] [[8 1] 464.78] [[8 2] 474.65] [[8 3] 484.27] [[8 4] 493.54] [[8 5] 502.38] [[8 6] 510.75] [[8 7] 518.62] [[8 8] 525.94] [[8 9] 532.58] [[8 10] 538.36]

;; ;; full 20x20 case
(for [i (partition (inc max-cars)  (sort final-sor))] (clojure.pprint/cl-format true "~{[~{[~{~2a ~1a~}] ~$~}]~} ~%" i))

(for [i (partition (inc max-cars)  (sort final-sor))] (clojure.pprint/cl-format true "~{~$ ~}~%" (map (fn[[[a,b] x]] x) i)))
;; 407.18 417.15 427.02 436.65 445.93 454.80 463.25 471.28 478.92 486.20 493.13 499.73 506.01 511.98 517.67 523.07 528.18 533.00 537.48 541.54 545.08 
;; 417.00 426.97 436.84 446.47 455.75 464.62 473.07 481.10 488.74 496.02 502.95 509.55 515.83 521.80 527.49 532.89 538.00 542.82 547.30 551.36 554.90 
;; 426.23 436.21 446.07 455.70 464.99 473.86 482.30 490.33 497.98 505.25 512.18 518.78 525.06 531.04 536.72 542.12 547.24 552.05 556.53 560.60 564.14 
;; 434.48 444.45 454.32 463.95 473.23 482.10 490.55 498.58 506.22 513.50 520.43 527.02 533.30 539.28 544.97 550.37 555.48 560.30 564.78 568.84 572.38 
;; 441.54 451.51 461.38 471.01 480.29 489.16 497.61 505.64 513.28 520.56 527.49 534.09 540.37 546.34 552.03 557.43 562.54 567.36 571.84 575.90 579.44 
;; 447.44 457.42 467.28 476.92 486.20 495.07 503.51 511.55 519.19 526.47 533.40 539.99 546.27 552.25 557.93 563.33 568.45 573.27 577.74 581.81 585.35 
;; 452.34 462.31 472.18 481.81 491.09 499.96 508.41 516.44 524.08 531.36 538.29 544.89 551.17 557.14 562.83 568.23 573.34 578.16 582.64 586.70 590.24 
;; 456.39 466.36 476.23 485.86 495.14 504.01 512.46 520.49 528.13 535.41 542.34 548.94 555.22 561.19 566.88 572.28 577.39 582.21 586.69 590.75 594.29 
;; 459.74 469.71 479.58 489.21 498.49 507.36 515.81 523.84 531.48 538.76 545.69 552.29 558.57 564.54 570.23 575.63 580.74 585.56 590.04 594.10 597.64 
;; 462.51 472.48 482.35 491.98 501.26 510.13 518.58 526.61 534.25 541.53 548.46 555.06 561.34 567.31 573.00 578.40 583.51 588.33 592.81 596.87 600.42 
;; 464.80 474.77 484.64 494.27 503.55 512.42 520.87 528.90 536.54 543.82 550.75 557.35 563.63 569.60 575.29 580.69 585.80 590.62 595.10 599.16 602.70 
;; 466.69 476.66 486.53 496.16 505.44 514.31 522.76 530.79 538.43 545.71 552.64 559.24 565.52 571.49 577.18 582.58 587.69 592.51 596.99 601.05 604.59 
;; 468.24 478.22 488.08 497.71 507.00 515.87 524.31 532.34 539.99 547.26 554.19 560.79 567.07 573.05 578.73 584.13 589.25 594.06 598.54 602.61 606.15 
;; 469.52 479.49 489.36 498.99 508.27 517.14 525.59 533.62 541.26 548.54 555.47 562.07 568.35 574.32 580.01 585.41 590.52 595.34 599.82 603.88 607.42 
;; 470.56 480.53 490.40 500.03 509.31 518.18 526.63 534.66 542.30 549.58 556.51 563.11 569.39 575.36 581.05 586.45 591.56 596.38 600.86 604.92 608.46 
;; 471.40 481.37 491.24 500.87 510.15 519.02 527.47 535.50 543.15 550.42 557.35 563.95 570.23 576.20 581.89 587.29 592.40 597.22 601.70 605.76 609.31 
;; 472.07 482.05 491.91 501.54 510.83 519.70 528.14 536.17 543.82 551.10 558.02 564.62 570.90 576.88 582.56 587.96 593.08 597.89 602.37 606.44 609.98 
;; 472.60 482.57 492.44 502.07 511.36 520.23 528.67 536.70 544.35 551.62 558.55 565.15 571.43 577.41 583.09 588.49 593.61 598.42 602.90 606.97 610.51 
;; 473.00 482.98 492.84 502.47 511.76 520.63 529.07 537.10 544.75 552.03 558.95 565.55 571.83 577.81 583.49 588.89 594.01 598.82 603.30 607.37 610.91 
;; 473.30 483.27 493.14 502.77 512.05 520.92 529.37 537.40 545.04 552.32 559.25 565.84 572.12 578.10 583.79 589.19 594.30 599.12 603.60 607.66 611.20 
;; 473.50 483.47 493.34 502.97 512.25 521.12 529.57 537.60 545.24 552.52 559.45 566.05 572.33 578.30 583.99 589.39 594.50 599.32 603.80 607.86 611.40 





(reverse (sort (map #(vector (update-val final-sor [5,0] %) %) (irange 5)))) ; ([438.2027232706664 4] [438.01415047390566 5] [436.47803867766083 3] [432.5689443325233 2] [426.478369053209 1] [418.48893511882125 0])

(defn permissible? [[a,b] action]
  (and (<= 0 (- a action) max-cars)
       (<= 0 (+ b action) max-cars)))

(permissible? [5,1] -4) ; false

(defn permissible-actions [[a,b]]
  (for [i (range -5 6) :when (permissible? [a,b] i)] i))

(permissible-actions [3,3]) ; (-2 -1 0 1 2) 
(permissible-actions [1,5]) ; (-4 -3 -2 -1 0)
(permissible-actions [5,1]) ; (0 1 2 3 4)

(defn actions [v [a,b]]
  (reverse (sort
            (map #(vector (update-val v [a,b] %) %)
                 (permissible-actions [a,b])))))

(actions final-sor [3,3]) ; ([441.75959450265043 2] [441.1080341068023 1] [437.93624386102465 0] [436.2655818847663 -1] [432.45037447215583 -2])

(defn optimal-action [v [a,b]]
  (second (first (actions v [a,b]))))

(optimal-action final-sor [3,3]) ; 2
(optimal-action final-sor [1,1]) ; 1
(optimal-action final-sor [0,0]) ; 0
(optimal-action final-sor [1,5]) ; -2
(optimal-action final-sor [5,1]) ; 4


(defn sor[policy omega] (fn [v] (reduce (sor-inplace omega) v (for [s states][s (policy s)]))))
(defn policyof[v] (into {} (for [s states] [s (optimal-action v s)])))


(println "--------------------hello----------------------------")

(def valuezero  (into {} (for [s states] [s 0])))
(def policyzero (into {} (for [s states] [s 0])))

(actions final-sor [1,1]) ; ([404.45998668491694 1] [403.36208923023344 -1] [402.4182186132667 0])



(pvalue valuezero)
(ppolicy policyzero)

(def vszero (iterate (sor policyzero 1.5) valuezero))

(take 10 (drop 30 (for [v vszero] (v [0,0]))))

(def valueone (nth vszero 40))

(pvalue valueone)

(actions valueone [20,20])


(def policyone (policyof valueone))

(ppolicy policyone)

(def vsone (iterate (sor policyone 1.5) valueone))

(ps 20 (for [v vsone] (v [0,0])))

(def valuetwo (nth vsone 20))

(pvalue valuetwo)

(def policytwo (policyof valuetwo))

(ppolicy policytwo)

(def vstwo (iterate (sor policytwo 1.5) valuetwo))

(ps 20 (for [v vstwo] (v [0,0])))


(def valuethree (nth vstwo 20))

(pvalue valuethree)

(def policythree (policyof valuethree))

(ppolicy policythree)

(def vsthree (iterate (sor policythree 1.5) valuethree))

(ps 20 (for [v vsthree] (v [0,0])))

(def valuefour (nth vsthree 20))

(pvalue valuefour)

(def policyfour (policyof valuefour))

(ppolicy policyfour)

(def vsfour (iterate (sor policyfour 1.5) valuefour))

(ps 20 (for [v vsfour] (v [0,0])))

(def valuefive (nth vsfour 20))

(pvalue valuefive)

(def policyfive (policyof valuefive))

(ppolicy policyfive)

(def vsfive (iterate (sor policyfive 1.5) valuefive))

(take 20 (for [v vsfive] (v [0,0])))

(def valuesix (nth vsfive 20))

(pvalue valuesix)

(def policysix (policyof valuesix))

(ppolicy policysix)

(= policyfive policysix) ; true












