;;nested maps and some ways of getting at the keys

(def me {:name
         {:firstname "John"
          :middlename "Lawrence"
          :surname "Aspden"}
         :address 
         {:street "Catherine Street"
          :town {:name "Cambridge"
                 :county "Cambridgeshire"
                 :country{
                          :name "England"
                          :history "Faded Imperial Power"
                          :role-in-world "Beating Australia at Cricket"}}}})


(:name me)
(get me :name)

(get-in me [:name :middlename])
(reduce get me [:address :town :country :role-in-world])
(-> me :address :town :county)

(assoc-in me [:name :initials] "JLA")
(update-in me [:address :street] #(str "33 " %))
