(defstruct item
  name
  size
  value)

(defun mk(name size value) (make-item :name name :value value :size size))

(defun total (list)
  (let ((total.size 0)(total.value 0))
  (dolist (x list)
    (incf total.size (item-size x))
    (incf total.value (item-value x)))
  (values total.size total.value)))


(defun addin (item l)
  (list (union (list item) l) l))

(defun all-selections (items)
  (if (= (length items) 1) `((,(car items))())
      (mapcan #'(lambda (x) (addin (car items) x)) (all-selections (cdr items)))))

(defun solves-problem (selection size budget)
  (multiple-value-bind (ts tv) (total selection)
      (and (<= ts size)(>= tv budget))))

(defun knapsack (size budget items)
  (mapcar (lambda (x) (if (solves-problem x size budget) x nil)) (all-selections items)))

(defvar *itemlist* (list
  (mk "Gold nugget"       3  20)
  (mk "Egg-sized-emerald" 4  30)
  (mk "Precious Jewelery" 8  30)
  (mk "Radio"             9  35)))

(dolist (i (sort (knapsack 10 30 *itemlist*) #'> :key (lambda (x) (cadr (multiple-value-list (total x))))))
  (unless (null i) (format t "-----~A size ~A~%" i (multiple-value-list (total i)))))
  