
;; Very strong, very clever villagers are more likely under the common cause model than under the independent mixed model and even less likely under the traditional model

(ptrad   {:str 18 :int 18}) ;-> 1/46656
(pindep  {:str 18 :int 18}) ;-> 25/746496

(< (ptrad {:str 18 :int 18}) (pindep {:str 18 :int 18}) (pcommon {:str 18 :int 18})) ;-> true




(def ltrad   (reduce * (map ptrad   village)))
(def lcommon (reduce * (map pcommon village)))
(def lindep  (reduce * (map pindep  village)))

(def posteriortrad   (/ ltrad   (+ ltrad lcommon lindep)))
(def posteriorcommon (/ lcommon (+ ltrad lcommon lindep)))
(def posteriorindep  (/ lindep  (+ ltrad lcommon lindep)))

(float posteriortrad) ;-> 0.15591037
(float posteriorcommon) ;-> 0.52879226
(float posteriorindep) ;-> 0.31529734


;; Clearly more research is needed!

(def city
  (binding [*randomizer* (java.util.Random. 0)]
    (doall (repeatedly 1000 (case (rand-int 3)
                    0 first-edition 
                    1 second-edition
                    2 third-edition)))))


(def ltrad   (Math/exp (+ 4970 (reduce + (map #(Math/log %) (map float (map ptrad city))))))))
(def lcommon (Math/exp (+ 4970 (reduce + (map #(Math/log %) (map float (map pcommon city)))))))
(def lindep  (Math/exp (+ 4970 (reduce + (map #(Math/log %) (map float (map pindep city)))))))

(def posteriortrad   (/ ltrad   (+ ltrad lcommon lindep)))
(def posteriorcommon (/ lcommon (+ ltrad lcommon lindep)))
(def posteriorindep  (/ lindep  (+ ltrad lcommon lindep)))

(float posteriortrad) ;-> 0.77889913
(float posteriorcommon) ;-> 0.046136353
(float posteriorindep) ;-> 0.17496453

(def country
  (binding [*randomizer* (java.util.Random. 0)]
    (doall (repeatedly 10000 (case (rand-int 3)
                    0 first-edition 
                    1 second-edition
                    2 third-edition)))))

(def lltrad   (reduce + (map #(Math/log %) (map float (map ptrad country)))))
(def llcommon (reduce + (map #(Math/log %) (map float (map pcommon country)))))
(def llindep  (reduce + (map #(Math/log %) (map float (map pindep country)))))

lltrad ; -49891.65823367781
llcommon ; -49941.17751169551
llindep ; -49935.172258139675

(def ltrad   (Math/exp (+ 49800 lltrad)))
(def lcommon (Math/exp (+ 49800 llcommon)))
(def lindep  (Math/exp (+ 49800 llindep)))

(def posteriortrad   (/ ltrad   (+ ltrad lcommon lindep)))
(def posteriorcommon (/ lcommon (+ ltrad lcommon lindep)))
(def posteriorindep  (/ lindep  (+ ltrad lcommon lindep)))

(float posteriortrad) ;-> 1.0
(float posteriorcommon) ;-> 3.1192545E-22
(float posteriorindep) ;-> 1.2650255E-19













;; (let [[t c i] (map (fn[pfn] (reduce * (map pfn city))) [ptrad pcommon pindep])
;;       sum (+ t c i)
;;       normalized [(/ t sum) (/ c sum) (/ i sum)]]
;;  (map float normalized))




