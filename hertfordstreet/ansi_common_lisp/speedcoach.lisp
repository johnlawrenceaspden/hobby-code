;;standard code to run tests on all the functions
(defparameter *testlist* nil)

(defmacro deftest(test result)
  `(setf *testlist* (cons (list ',result (lambda() ,test) ',test) *testlist*)))

(defun runtests()
  (let ((acc nil))
    (dolist (i *testlist*)
      (format t "test: ~A (expects ~A) ~%" (caddr i) (car i))
      (let ((x (funcall (cadr i))))
        (format t "=> ~A " x)
        (if (equal x (car i))
            (progn
              (format t "(PASS)~%")
              (setf acc (cons x acc)))
            (progn
              (format t "(FAIL)")
              (setf acc 'TESTS-FAILED!!!)
              (return)))))
    acc))

;;end of test code

;;;;Rowing Calculations
;;;;All units are in metres, kilograms and seconds

;;;;conversion code for weights in imperial, 1:40 split <-> 5 m/s etc
(defun minsandsecsfromseconds (seconds)
  (let* ((mins (floor (/ seconds 60)))
	 (seconds (- seconds (* mins 60))))
    (values mins seconds)))

(deftest (multiple-value-list (minsandsecsfromseconds 100)) (1 40))

(defun secondsfromminsandsecs (min secs)
  (+ (* min 60) secs))

(defun floorminsandsecsfromseconds (seconds)
  (multiple-value-bind (m s) (minsandsecsfromseconds seconds)
    (values m (floor s))))

(defun speedfromsplit (min sec)
  (/ 500 (+ sec (* min 60))))

(deftest (speedfromsplit 1 40) 5)
(deftest (speedfromsplit 2 00) 500/120)

(defun weight (&key (st 0) (lb 0) (kg 0))
  (+ kg 
     ( / (+ (* st 14) lb) 22/10)))
  
(deftest (weight :lb 170) 850/11)
(deftest (weight :st 10 :lb 8) 740/11)
(deftest (weight :kg 90) 90)

(defun timefromsplitanddistance (split distance)
  (/ distance split))

(deftest (timefromsplitanddistance (speedfromsplit 1 40) 500) 100)


(defconstant LENGTH_OF_HEAD_COURSE 2600)

(defun head-course-time (splitmins splitsecs &key (flow 0))
  (floorminsandsecsfromseconds 
   (timefromsplitanddistance (speedfromsplit splitmins splitsecs) 
			     (+ LENGTH_OF_HEAD_COURSE flow))))

(deftest (multiple-value-list (head-course-time 1 40)) (8 40))
(deftest (multiple-value-list (head-course-time 2 00)) (10 24))
(deftest (multiple-value-list (head-course-time 1 50)) (9 32))

;;assume that a standard VIII weighs 110 kgs
;;assume its oars weigh 10 kgs
;;assume a standard cox weighs 50 kgs
(defconstant DEADWEIGHT-PER-MAN (/ (+ 110 10 50) 8))

;;I believe that the speed of the boat scales with the 1/6th power
;;of the weight for fixed power
(defconstant ADJUSTING-EXPONENT 1/6)

;;On the Concept II website, they assert that 8 170lb rowers
;;rowing perfectly, each with a 7:00/2k erg score
;;will achieve a 2k time of 6:19.

(defun magic-factor (weight)
  (let ((fiddle-factor  ( / (secondsfromminsandsecs 6 19)
			    (secondsfromminsandsecs 7 00)))
	(standardweight (weight :lb 170)))

    (* fiddle-factor 
       (expt (/ (+ weight         DEADWEIGHT-PER-MAN)
		(+ standardweight DEADWEIGHT-PER-MAN))
	     ADJUSTING-EXPONENT))))

(deftest (magic-factor (weight :lb 170)) 0.90238094)

(defun ergo-weight-adjust-time (secs weight)
  (* secs (magic-factor weight)))

(deftest (multiple-value-list 
	  (floorminsandsecsfromseconds 
	   (ergo-weight-adjust-time (secondsfromminsandsecs 7 00)
				    (weight :lb 170))))
    (6 19))

(defun ergo-weight-adjust-distance (m weight)
  (/ m (magic-factor weight)))


(defmacro ewatestmetric (score st lb adjustedscore)
  `(deftest (ergo-weight-adjust-distance ,score (weight :st ,st :lb ,lb)) ,adjustedscore))

(defun ewadi (score st lb)  (ergo-weight-adjust-distance score (weight :st st :lb lb)))
(defun ewadm (score kg) (ergo-weight-adjust-distance score (weight :kg kg)))

(deftest (ewadi 8283 12 11) 9115) ;sips
(deftest (ewadm 8284 90) 8994)    ;braithwaite
(deftest (ewadi 8150 

		Metcalfe 12 11 8283
		Braithwaite 90 8284
		Watt 12 10     8150
		Wood 165 7796
		Twigg 78  7792
		Southgate 12 10 7814
		Aspden 13 12 7850
		Howard 90 7636
		Hurst K 11 10 7250
		Test Bloke 170 8000


  
(magic-factor (float 895/11))

(defclass ergoscore()
  ((distance :initarg :distance)
   (time :initarg :time)
   (date :initarg :date :initform "unknown")))

(defclass crewman()
  ((name     :initarg :name)
   (weight   :initarg :weight)
   (ergolist :initarg :erglist :initform nil)))

(make-instance 'crewman :name "Metcalfe" 
	       :weight (weight :st 12 :lb 11)
	       :erglist (list 
			 (make-instance 'ergoscore :distance 8283 
					:time (* 30 60) 
					:date "14/06/2007")))




(head-course-time 1 35 :flow 00)

(runtests)




