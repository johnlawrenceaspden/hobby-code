;; I wanted a bidirectional map, to take unique userids to usernames

;; Clojure doesn't have an appropriate data structure.

;; In fact I want it to be slightly asymmetrical. I want to be able to
;; add new userids, and change the usernames associated with existing
;; userids, but I don't want to be able to change the userids of existing
;; values.

;; We can make an appropriate table out of two ordinary maps

(def empty-bidirectional-map [{}{}])

;; I want assoc to change the values associated with keys as it would
;; ordinarily, but never to allow two keys to point to the same value
;; or to change the key associated with an existing value

(defn assoc-unique [[forwardmap backmap] userid name]
  (if (nil? (backmap name))
    (let [oldname (forwardmap userid)]
      [(assoc forwardmap userid name) (assoc (dissoc backmap oldname) name userid)])
    (throw (Exception. "Non-unique username"))))

(defn get-name-from-userid [m userid] ((first m) userid))
(defn get-userid-from-name [m name]   ((second m) name))

;; Here's a test map
(def test-map
  ( -> empty-bidirectional-map
       (assoc-unique  "identity"  "name")
       (assoc-unique  "identity"  "newname" )
       (assoc-unique  "identity2" "name2")
       (assoc-unique  "identity2" "name")))

(assert (and
         (= (get-name-from-userid test-map "identity") "newname")
         (= (get-name-from-userid test-map "identity2") "name")
         (= (get-userid-from-name test-map "newname") "identity" )
         (= (get-userid-from-name test-map "name") "identity2" )
         (= (get-userid-from-name test-map "name2") nil)
         (= (get-name-from-userid test-map "ident") nil)
         (= (get-name-from-userid test-map "ident") nil)
         (= "Non-unique username" (try 
                                    (assoc-unique test-map "identity3" "newname")
                                    (catch Exception e (.getMessage e) )))))






(defonce users (atom (assoc-unique [{}{}] "identity" "name")))





