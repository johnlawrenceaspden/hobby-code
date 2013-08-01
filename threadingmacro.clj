;; robbed with thanks off of:
;; http://bryangilbert.com/code/2013/07/30/anatomy-of-a-clojure-macro/

(defn replace-if-underscore [element val] 
  (if (= element '_) val element))

(defn replace-underscores [form val]
      (map #(replace-if-underscore % val) form))

(defn convert-forms [val [next-form & other-forms]]               
      (if (nil? next-form)                                         
        val                                                           
        (let [next-val (gensym)]                                   
          `(let [~next-val ~(replace-underscores next-form val)]   
             ~(convert-forms next-val other-forms)))))  

(defmacro ->>> [init & forms]
      (convert-forms init forms))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(->>> 20 
      (* _ _ _)
      (* _ _ _)) ;-> 512000000000

(reduce * (repeat 9 20 )) ;-> 512000000000


(->>> #{}
      (conj _ 'doom)
      (conj _ 'horror)
      (_ 'treason)) ;-> nil

(->>> #{}
      (conj _ 'doom)
      (conj _ 'horror)
      (_ 'horror)) ;-> horror

;; Awesome. Votes for this in the standard library!