/* Two anti cancer drugs on a sample produce simpson's paradox */

(let [mpl 200  mpd 800
      mnl 15  mnd 95
      fpl 40  fpd 60
      fnl 350  fnd 650] 
  (let [pl (+ mpl fpl) pd (+ mpd fpd)
        nl (+ mnl fnl) nd (+ mnd fnd)]
    (let [ns (/ nl (+ nl nd))
          ps (/ pl (+ pl pd))]
      (print "placebifen live" pl "\n")
      (print "placebifen die " pd "\n")
      (print "nogudox live"    nl "\n")
      (print "nogudox die " nd "\n")
      (print "nogudox:placebifen "
             ns "(" (float ns)") : "
             ps "(" (float ps) ")\n"))))

;; placebifen live 240 
;; placebifen die  860 
;; nogudox live 365 
;; nogudox die  745 
;; nogudox:placebifen  73/222 ( 0.32882884 ) :  12/55 ( 0.21818182 )


(let [mpl 40  mpd 160
      mnl 15  mnd 85
      fpl 40  fpd 60
      fnl 70  fnd 130] 
  (let [pl (+ mpl fpl) pd (+ mpd fpd)
        nl (+ mnl fnl) nd (+ mnd fnd)]
    (let [ns (/ nl (+ nl nd))
          ps (/ pl (+ pl pd))]
      (print "placebifen live" pl "\n")
      (print "placebifen die " pd "\n")
      (print "nogudox live"    nl "\n")
      (print "nogudox die " nd "\n")
      (print "nogudox:placebifen "
             ns "(" (float ns)") : "
             ps "(" (float ps) ")\n"))))

;; placebifen live 80 
;; placebifen die  220 
;; nogudox live 85 
;; nogudox die  215 
;; nogudox:placebifen  17/60 ( 0.28333333 ) :  4/15 ( 0.26666668 )
