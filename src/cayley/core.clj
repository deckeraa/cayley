(ns cayley.core
  (:use [clojure.test :as test]))

;; The protocol for defining a group
(defprotocol group
  "Protocol for defining elements and how the elements operate in a group"
  (elems [this] "Returns a set of the elements of the group")
  (operate-internal [this elem1 elem2] "Operates elem1 with elem2")
  (inverse [this elem] "Returns the inverse of elem")
  (ident [this] "Returns the identity element"))

;; Internal functions for use with Cayley tables
(defn- operate-via-cayley-table [table elem1 elem2]
  (get (get table elem1) elem2))

(defn- invert-via-cayley-table [table elem]
  (:iota (clojure.set/map-invert (get table elem))))


;; This is a workaround to define a multi-variadic operate function
;; for all groups. It's pretty clean for a work-around since, when
;; called, it looks just like it could belong to the group protocol.
(def operate
  (fn this
    ([group elem1 elem2]
       (operate-internal group elem1 elem2))
    ([group elem1 elem2 & more-elems]
       (apply this group (this group elem1 elem2) more-elems))))

;; d4 -- symmetries of a square, using my abstract algebra professor's custom notation
(def d4-elems #{:iota :phi :psi :theta :sigma :tau :lambda :mu} )
(def d4-cayley
  {:iota {:iota :iota, :phi :phi, :psi :psi, :theta :theta, :sigma :sigma, :tau :tau, :lambda :lambda, :mu :mu }
   :phi  {:iota :phi, :phi :iota, :psi :theta, :theta :psi, :sigma :tau, :tau :sigma, :lambda :mu, :mu :lambda }
   :psi  {:iota :psi, :phi :theta, :psi :iota, :theta :phi, :sigma :mu, :tau :lambda, :lambda :tau, :mu :sigma }
   :theta  {:iota :theta, :phi :psi, :psi :phi, :theta :iota, :sigma :lambda, :tau :mu, :lambda :sigma, :mu :tau }
   :sigma {:iota :sigma, :phi :tau, :psi :lambda, :theta :mu, :sigma :phi, :tau :iota, :lambda :theta :mu :psi }
   :tau {:iota :tau, :phi :sigma, :psi :mu, :theta :lambda, :sigma :iota, :tau :phi, :lambda :psi, :mu :theta }
   :lambda {:iota :lambda, :phi :mu, :psi :sigma, :theta :tau, :sigma :psi, :tau :theta, :lambda :iota, :mu :phi }
   :mu {:iota :mu, :phi :lambda, :psi :tau, :theta :sigma, :sigma :theta, :tau :psi, :lambda :phi, :mu :iota }})

(defrecord d4 []
  group
  (elems [this] #{:iota :phi :psi :theta :sigma :tau :lambda :mu})
  (operate-internal [this elem1 elem2] (operate-via-cayley-table d4-cayley elem1 elem2))
  (inverse [this elem] (invert-via-cayley-table d4-cayley elem))
  (ident [this] :iota))

(deftest test-d4-elem (is (= (elems (d4.)) #{:iota :phi :psi :theta :sigma :tau :lambda :mu})))
(deftest test-d4-1 (is (= (operate (d4.) :sigma :tau) :iota)))
(deftest test-d4-group
  (testing "d4 -- symmetries of a square"
    (testing "identity"
      (is (= (ident (d4.)) :iota)))))

;; integer groups -- addition modulo n
(defrecord int-group [n]
  group
  (elems [this]
    (set (range 0 n)))
  (operate-internal [this elem1 elem2]
    (mod (+ elem1 elem2) n))
  (inverse [this elem]
    (if (= 0 elem)
      0
      (- n elem)))
  (ident [this] 0))

(deftest test-int-group
  (testing "int-group -- addition modulo n"
    (testing "elems"
      (is (= (elems (int-group. 3)) #{0 1 2}))
      (is (= (elems (int-group. 1)) #{0})))
    (testing "operate"
      (is (= (operate (int-group. 4) 3 2)  1))
      (is (= (operate (int-group. 5) 3 2)  0)))
    (testing "inverse"
      (is (= (inverse (int-group. 4) 3) 1))
      (is (= (inverse (int-group. 2349532) 0) 0)))
    (testing "identity"
      (is (= (ident (int-group. 168)) 0)))))


(defn conjugate "Conjugate elem1 with elem2: elem2*elem1*elem2^{-1}" [group elem1 elem2]
  (operate group elem2 elem1 (inverse group elem2))
  )

(deftest test-conjugate
  (testing "conjugation on d4"
    (is (= (conjugate (d4.) :sigma :theta) :tau))
    (is (= (conjugate (d4.) :psi :mu) :theta)))
  (testing "conjugation on Z_4"
    (is (= (conjugate (int-group. 4) 2 3) 2)
        (= (conjugate (int-group. 4) 1 1) 3))))

(defn- gen-single-conjugation-map [table n G]
  (map #(list (str n " conj " %1) (conjugate table n %1)) G))

(defn- gen-conjugation-maps
  "Creates a "
  [table N G]
  (mapcat #(gen-single-conjugation-map table %1 G) N))

(defn normal? "Returns true if set N is normal in set G, returns a list of counterexamples otherwise. N and G must be sets."
  [table N G]
  (let [counterexamples (filter #(not (contains? N (second %1)))
                                (gen-conjugation-maps table N G))]
    (if (empty? counterexamples)
      true
      counterexamples)))


;; (defn cycle-to-set "Returns the cycle constructed by the element over the operation given by table" [table element]
;;   (set (cons :iota
;;              (take-while #(not= :iota %1)
;;                          (rest (iterate #(star table %1 element) :iota))))))

(defn- operate-many [table elements]
  (set (for [x elements y elements]
         (star table x y))))

(defn cycle-to-set [table elements]
  (let [newly-found (clojure.set/union
                     elements
                     (operate-many table elements))]
    (if (= elements newly-found)
      elements
      (cycle-to-set table newly-found))))

;; Evaluate these for 21.22
;; (normal? d4-cayley (cycle-to-set d4-cayley #{:iota}) d4-elems)
;; (normal? d4-cayley (cycle-to-set d4-cayley #{:theta}) d4-elems)
;; (normal? d4-cayley (cycle-to-set d4-cayley #{:psi}) d4-elems)
;; (normal? d4-cayley (cycle-to-set d4-cayley #{:phi}) d4-elems)
;; (normal? d4-cayley (cycle-to-set d4-cayley #{:lambda}) d4-elems)
;; (normal? d4-cayley (cycle-to-set d4-cayley #{:mu}) d4-elems)
;; (normal? d4-cayley (cycle-to-set d4-cayley #{:theta :psi}) d4-elems)
;; (normal? d4-cayley (cycle-to-set d4-cayley #{:tau}) d4-elems)
;; (normal? d4-cayley (cycle-to-set d4-cayley #{:lambda :mu}) d4-elems)
;; (normal? d4-cayley (cycle-to-set d4-cayley d4-elems) d4-elems)

;; playground -- these should really get turned into unit tests somehow
;; (map #(list (str :theta " conj " %1) (conjugate d4-cayley :theta %1)) d4-elems)
;; (gen-single-conjugation-map d4-cayley :theta d4-elems)
;; (gen-conjugation-maps d4-cayley #{:theta :psi} d4-elems)
;; (normal? d4-cayley #{:theta} d4-elems)
;; (normal? d4-cayley (cycle-to-set d4-cayley :theta) d4-elems)
;; (conjugate d4-cayley :psi :lambda)
;; (contains? #{:theta} :theta)

;; (star d4-cayley :lambda :tau)
;; (star d4-cayley :sigma :theta :tau)
;; (star d4-cayley :lambda :psi :lambda)
;; (conjugate d4-cayley :theta :tau)
;; (star d4-cayley :tau :theta :sigma)
;; ((partial star d4-cayley) :tau :lambda)

;; (inverse d4-cayley :tau)

;; (take-while #(not= :iota %1) (rest (iterate #(star d4-cayley %1 :theta) :iota)))
;; (cycle-to-set d4-cayley :tau)

;; (clojure.set/union (cycle-to-set d4-cayley :theta) (cycle-to-set d4-cayley :tau))
;; (for [x #{:tau :sigma}
;;       y #{:iota :phi}]
;;   (star d4-cayley x y))
;; (operate-many d4-cayley (operate-many d4-cayley #{:tau :phi}))
;; (cycle-to-set d4-cayley #{:lambda :mu})
