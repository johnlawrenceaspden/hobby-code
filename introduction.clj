(def visitors (ref #{}))

(defn hello [username]
  (dosync
   (let [past-visitor (@visitors username)]
     (if past-visitor
       (str "Welcome back " username)
       (do
	 (alter visitors conj username)
	 (str "Hello " username))))))

(defn demo[]
  (map hello ["Fred" "John" "John"]))

;(demo)












