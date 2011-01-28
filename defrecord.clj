;; I'm not at all sure what defrecord is for. Just playing with it here.
;; Examples from Stuart Halloway's talk: http://www.infoq.com/presentations/Clojure-Java-Interop

(defrecord Foo [a b c]) ; user.Foo      ; defines a java class?

(def f (Foo. 1 2 3)) ; #'user/f         ; creates an instance of that class
                                        ; perhaps the class has a three-argument constructor



f ; #:user.Foo{:a 1, :b 2, :c 3}     ; look like a kind of map with keys :a :b :c

;; Accessors

(:a f) ; 1
(:b f) ; 2
(:c f) ; 3

;; More accessors
(.a f) ; 1
(.b f) ; 2
(.c f) ; 3

;; Which are just java things
(. f a) ; 1
(. f b) ; 2
(. f c) ; 3

;; Countable
(count f) ; 3

;; A sequence
(first f) ; [:a 1]
(rest f) ; ([:b 2] [:c 3])
(first (rest f)) ; [:b 2]
(rest (rest f)) ; ([:c 3])

(seq f) ; ([:a 1] [:b 2] [:c 3])

;; A bona fide java class:

(class f) ; user.Foo

;; With superclasses

(supers (class f)) ; cripes
#{clojure.lang.IPersistentMap  
  clojure.lang.Counted
  clojure.lang.Associative
  clojure.lang.IObj
  clojure.lang.IMeta
  clojure.lang.Seqable
  clojure.lang.IPersistentCollection
  java.io.Serializable
  clojure.lang.IKeywordLookup
  java.lang.Object
  clojure.lang.ILookup
  java.lang.Iterable
  java.util.Map}

;; And methods

(defn meths [x] (println (apply str (interpose "\n" (map str (.getMethods (if (class? x) x (class x))))))))

(meths f)
public clojure.lang.IObj user.Foo.withMeta(clojure.lang.IPersistentMap)
public boolean user.Foo.equiv(java.lang.Object)
public clojure.lang.IPersistentMap user.Foo.assoc(java.lang.Object,java.lang.Object)
public clojure.lang.Associative user.Foo.assoc(java.lang.Object,java.lang.Object) throws java.lang.Exception
public java.lang.Object user.Foo.valAt(java.lang.Object)
public java.lang.Object user.Foo.valAt(java.lang.Object,java.lang.Object)
public clojure.lang.IMapEntry user.Foo.entryAt(java.lang.Object)
public clojure.lang.IPersistentMap user.Foo.without(java.lang.Object) throws java.lang.Exception
public clojure.lang.ILookupThunk user.Foo.getLookupThunk(clojure.lang.Keyword)
public java.lang.Object user.Foo.get(java.lang.Object)
public java.lang.Object user.Foo.put(java.lang.Object,java.lang.Object)
public boolean user.Foo.equals(java.lang.Object)
public java.util.Collection user.Foo.values()
public int user.Foo.hashCode()
public void user.Foo.clear()
public int user.Foo.count()
public int user.Foo.size()
public boolean user.Foo.isEmpty()
public clojure.lang.IPersistentCollection user.Foo.empty()
public java.util.Set user.Foo.entrySet()
public void user.Foo.putAll(java.util.Map)
public clojure.lang.IPersistentCollection user.Foo.cons(java.lang.Object)
public java.lang.Object user.Foo.remove(java.lang.Object)
public java.util.Set user.Foo.keySet()
public boolean user.Foo.containsValue(java.lang.Object)
public boolean user.Foo.containsKey(java.lang.Object)
public clojure.lang.ISeq user.Foo.seq()
public clojure.lang.IPersistentMap user.Foo.meta()
public final native void java.lang.Object.wait(long) throws java.lang.InterruptedException
public final void java.lang.Object.wait(long,int) throws java.lang.InterruptedException
public final void java.lang.Object.wait() throws java.lang.InterruptedException
public java.lang.String java.lang.Object.toString()
public final native java.lang.Class java.lang.Object.getClass()
public final native void java.lang.Object.notify()
public final native void java.lang.Object.notifyAll()
public abstract clojure.lang.IPersistentMap clojure.lang.IPersistentMap.assocEx(java.lang.Object,java.lang.Object) throws java.lang.Exception
public abstract java.util.Iterator java.lang.Iterable.iterator()

(meths clojure.lang.ILookup)
                                        ; public abstract java.lang.Object clojure.lang.ILookup.valAt(java.lang.Object)
                                        ; public abstract java.lang.Object clojure.lang.ILookup.valAt(java.lang.Object,java.lang.Object)

;; Yet more ways to get at the data
(. f valAt :a) ; 1
(. f valAt :b) ; 2
(. f valAt :c) ; 3

(. f valAt :d) ; nil
(. f valAt :d 1) ; 1

;; Here's Clojure's Data-Oriented Style for a person with an address

(def jla '{:name John :surname Aspden :address {:number 3 :street "Round Church Street" :town "Cambridge"}})

jla ; {:name John, :surname Aspden, :address {:number 3, :street "Round Church Street", :town "Cambridge"}}

(:name jla) ; John

(-> jla :address :street) ; "Round Church Street"
(:street (:address jla)) ; "Round Church Street"
(get-in jla [:address :street]) ; "Round Church Street"

(assoc jla :name "John Lawrence") ; {:name "John Lawrence", :surname Aspden, :address {:number 3, :street "Round Church Street", :town "Cambridge"}}
(assoc-in jla [:address :street] "Catharine Street") ; {:name John, :surname Aspden, :address {:number 3, :street "Catharine Street", :town "Cambridge"}}

(update-in jla [:address :street] #(. % toUpperCase)) ; {:name John, :surname Aspden, :address {:number 3, :street "ROUND CHURCH STREET", :town "Cambridge"}}


;; Here's the defrecord Object-Oriented version

(defrecord Person [name surname address]) ; user.Person
(defrecord Address [number street town]) ; user.Address

(def jla (Person. 'John 'Aspden (Address. 3 "Round Church Street" "Cambridge"))) ; #'user/jla

jla ; #:user.Person{:name John, :surname Aspden, :address #:user.Address{:number 3, :street "Round Church Street", :town "Cambridge"}}

(-> jla :address :street) ; "Round Church Street"
(update-in jla [:address :street] #(. % toUpperCase)) ; #:user.Person{:name John, :surname Aspden, :address #:user.Address{:number 3, :street "ROUND CHURCH STREET", :town "Cambridge"}}

;; Records are still associative maps, even though they're also java classes
