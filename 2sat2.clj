;; 2-SAT

;; Let's generalize our problem a bit:

;; The instance of 2-SAT from last time:

;; Either x1 must be true or x2 must be true
;; Either x3 must be true or x4 must be true
;; Either x1 must be false or x3 must be false
;; Either x2 must be false or x4 must be false

;; in a more general form might look like:

(def problem [{:x1 true  :x2 true}
              {:x3 true  :x4 true}
              {:x1 false :x3 false}
              {:x2 false :x4 false}])

;; take an example clause 

(def example-clause {:x1 true :x2 true})

;; and a sample variable assignment

(def sample-assignment {:x1 true :x2 false :x3 true :x4 false})

;; and we can see whether the assignment satisfies either of the 
;; conditions in the clause

(for [[k v] example-clause] 
  (= (sample-assignment k) v)) ;-> (false true)

;; In this case, the clause says that either variable 1 must be true
;; (which is true) or variable 2 must be true (which is false).  Since
;; one of the clauses' conditions is satisfied, this clause overall is happy.


;; That allows us to make a function that tells us whether an
;; assignment satisfies a particular clause.
(defn satisfies [assignment clause]
  (some identity (for [[k v] clause] 
                   (= (assignment k) v))))


(satisfies sample-assignment example-clause) ;-> true

;; We can try each of the clauses in our example problem on our sample assignment
(satisfies sample-assignment {:x1 true  :x2 true}) ;-> true
(satisfies sample-assignment {:x3 true  :x4 true}) ;-> true
(satisfies sample-assignment {:x1 false :x3 false}) ;-> nil
(satisfies sample-assignment {:x2 false :x4 false}) ;-> true

;; And we find as before that it satisfies the first, second and
;; fourth clauses but not the third one.

;; Another way to do that would be:
(map (partial satisfies sample-assignment) problem ) ;-> (true true nil true)

;; And so a way to check whether they're all satisfied at once is:
(every? identity (map (partial satisfies sample-assignment) problem)) ;-> false

;; So here's a general mechanism to check whether an assignment is a
;; solution to a problem
(defn solves [assignment problem]
  (every? identity 
          (map (partial satisfies assignment) problem)))

(solves sample-assignment problem) ;-> false

;; Just to check, here's an assignment that isn't a solution
(solves {:x1 true :x2 false :x3 true :x4 false} problem) ;-> false
;; And here's one that is
(solves {:x1 true :x2 false :x3 false :x4 true} problem) ;-> true

;; How should we do our exhaustive search now?

;; Firstly find all the variables mentioned in the problem
(distinct (mapcat keys problem)) ;-> (:x2 :x1 :x4 :x3)

;; Then find a way of generating all possible assignments of true and false to 
;; those variables
(defn all-assignments [vars]
  (if (empty? vars) '({})
      (apply concat (for [a (all-assignments (rest vars))]
                      (list (assoc a (first vars) true)
                            (assoc a (first vars) false))))))
        
      

(all-assignments '()) ;-> ({})
(all-assignments '(:x1)) ;-> ({:x1 true} {:x1 false})
(all-assignments '(:x1 :x2)) ;-> ({:x1 true, :x2 true} {:x1 false, :x2 true} {:x1 true, :x2 false} {:x1 false, :x2 false})
(all-assignments '(:x1 :x2 :x3)) ;-> ({:x1 true, :x2 true, :x3 true} {:x1 false, :x2 true, :x3 true} {:x1 true, :x2 false, :x3 true} {:x1 false, :x2 false, :x3 true} {:x1 true, :x2 true, :x3 false} {:x1 false, :x2 true, :x3 false} {:x1 true, :x2 false, :x3 false} {:x1 false, :x2 false, :x3 false})

;; This gets back the two possible satisfying assignments that we worked out earlier:
(filter #(solves % problem) (all-assignments (distinct (mapcat keys problem))))
;; {:x2 true, :x1 false, :x4 false, :x3 true} 
;; {:x2 false, :x1 true, :x4 true, :x3 false}


(defn exhaustive-search-solve [problem]
  (filter #(solves % problem) 
          (all-assignments (distinct (mapcat keys problem)))))


;; We can now generate some test cases for future use:

;; If we just say that either x1 or x2 needs to be true
(exhaustive-search-solve
 [{:x1 true  :x2 true}]) 

;; Then there are three ways of doing that
;-> ({:x2 true, :x1 true} {:x2 false, :x1 true} {:x2 true, :x1 false})

;; But if we want one of them true and one of them false
(exhaustive-search-solve
 [{:x1 true  :x2 true} {:x1 false  :x2 false}]) 
;-> ({:x2 false, :x1 true} {:x2 true, :x1 false})

;; Then there are only two ways
(exhaustive-search-solve
 [{:x1 true  :x2 true} {:x1 false  :x2 false}])















