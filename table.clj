;; Brian Carper's table printer, nicked off his blog
;; http://briancarper.net/tag/157/clojure

(defn table
  "Given a seq of hash-maps, prints a plaintext table of the values of the hash-maps.
  If passed a list of keys, displays only those keys.  Otherwise displays all the
  keys in the first hash-map in the seq."
  ([xs]
     (table xs (keys (first xs))))
  ([xs ks]
     (when (seq xs)
       (let [f (fn [old-widths x]
                 (reduce (fn [new-widths k]
                           (let [length (inc (count (str (k x))))]
                             (if (> length (k new-widths 0))
                               (assoc new-widths k length)
                               new-widths)))
                         old-widths ks))
             widths (reduce f {} (conj xs (zipmap ks ks)))
             total-width (reduce + (vals widths))
             format-string (str "~{"
                                (reduce #(str %1 "~" (%2 widths) "A") "" ks)
                                "~}~%")]
         (cl-format true format-string (map str ks))
         (cl-format true "~{~A~}~%" (repeat total-width \-))
         (doseq [x xs]
           (cl-format true format-string (map x ks)))))))


(table [])
nil
(table [{:name "John"}])
:name 
------
John  
nil
(table [{:name "John" :age 40}])
:name :age 
-----------
John  40   
nil
(table [{:name "John" :age 40 :sex 'm}{:name "Susie" :age 27 :sex 'f}])
:name :age :sex 
----------------
John  40   m    
Susie 27   f    
nil
;;hmm, doesn't do the union.
(table [{:name "John" :sex 'm}{:name "Susie" :age 27 }])
:name :sex 
-----------
John  m    
Susie nil  
nil

(table [{:doom 4 :name "Susie"}{:name "John" :doom 5}] [:name])
:name 
------
Susie 
John  
nil

(table [{:name "John" :sex 'm}{:name "Susie" :age 27 }][:name :age :sex])
:name :age :sex 
----------------
John  nil  m    
Susie 27   nil  
nil

(table [{:name "John" :sex 'm}{:name "Jehosaphat" :age 27 }][:name :age :sex])
(table [{:name "John"}{:name "Jehosaphat" :age 27 }][:name :age :sex])
