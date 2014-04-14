(ns cayley.core
  (:use [clojure.test :as test]
        [clojure.math.combinatorics :as combo]))

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


;; This is a workaround to define a variadic operate function
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

;; integer groups -- addition modulo n
(defrecord direct-product [group1 group2]
  group
  (elems [this]
    (set (combo/cartesian-product (elems group1) (elems group2))))
  (operate-internal [this elem1 elem2]
    (list
     (operate group1 (first  elem1) (first  elem2))
     (operate group2 (second elem1) (second elem2))))
  (inverse [this elem]
    (list
     (inverse group1 (first elem))
     (inverse group2 (second elem))))
  (ident [this]
    (list
     (ident group1)
     (ident group2))))

(deftest test-direct-product
  (testing "Z_4 x Z_2"
    (is (= (elems (direct-product. (int-group. 4) (int-group. 2)))
           #{'(0 0) '(0 1) '(1 0) '(1 1) '(2 0) '(2 1) '(3 0) '(3 1)}))))

(defn- operate-many
  "Operates all n elements in the group together, to yield n^2 elements"
  [group elements]
  (set (for [x elements y elements]
         (operate group x y))))

;; Operates all elements together repeatedly until no new elements are
;; found, at which point it is known that the elements are closed over
;; the operation. Basically finds the fixed-point of operation over a set.
(defn cycle-to-set "Takes cycle as a set of elements in a group and returns the set of elements in the cycles."
  [group elements]
  (let [newly-found (clojure.set/union
                     elements
                     (operate-many group elements))]
    (if (= elements newly-found)
      elements
      (cycle-to-set group newly-found))))

(deftest test-cycle-to-set
  (testing "cycle-to-set on Z_4"
    (is (= (cycle-to-set (int-group. 4) #{0}) #{0}))
    (is (= (cycle-to-set (int-group. 4) #{1}) #{0 1 2 3}))
    (is (= (cycle-to-set (int-group. 4) #{2}) #{0 2}))
    (is (= (cycle-to-set (int-group. 4) #{3}) #{3 2 1 0}))
    (is (= (cycle-to-set (int-group. 4) #{0 1}) #{0 1 2 3}))))

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

(defn- gen-single-conjugation-map [group n G]
  (map #(list (str n " conj " %1) (conjugate group n %1)) G))

(defn- gen-conjugation-maps
  [group N G]
  (mapcat #(gen-single-conjugation-map group %1 G) N))

(defn normal? "Returns true if set N is normal in set G, returns a list of counterexamples otherwise. N and G must be sets."
  ([N group]
     (normal? group N (elems group)))
  ([group N G]
      (let [counterexamples (filter #(not (contains? N (second %1)))
                                    (gen-conjugation-maps group N G))]
        (if (empty? counterexamples)
          true
          counterexamples)))
  )

;; Since normal? returns counterexamples if it the group is not normal,
;; I am testing the non-normal groups to see if they do not return true,
;; since I haven't defined which counterexamples should be returned.
(deftest test-normal?
  (testing "normality on d4"
    (is (= (normal? (cycle-to-set (d4.) #{:iota}) (d4.)) true))
    (is (not (= (normal? (cycle-to-set (d4.) #{:theta}) (d4.)) true)))
    (is (not (= (normal? (cycle-to-set (d4.) #{:psi}) (d4.)) false)))
    (is (= (normal? (cycle-to-set (d4.) #{:phi}) (d4.)) true))
    (is (not (= (normal? (cycle-to-set (d4.) #{:lambda}) (d4.)) true)))
    (is (not (= (normal? (cycle-to-set (d4.) #{:mu}) (d4.)) true)))
    (is (= (normal? (cycle-to-set (d4.) #{:theta :psi}) (d4.)) true))
    (is (= (normal? (cycle-to-set (d4.) #{:tau}) (d4.)) true))
    (is (= (normal? (cycle-to-set (d4.) #{:lambda :mu}) (d4.)) true))
    (is (= (normal? (cycle-to-set (d4.) d4-elems) (d4.)) true))
    ))

(defn generate-subgroups-naive
  "Returns a set of sets of elements that constitute subgroups of the passed group."
  [group]
  (->>
   (combo/subsets (elems group)) ;; generate subsets (come out as seqs)
   (remove empty?)
   (map set) ;; make the seqs into sets
   (map #(cycle-to-set group %1)) ;; expand the cycles into sets
   (set) ;; convert the whole thing to a set
   ))

(deftest test-generate-subgroups-naive
  (testing "subgroups on int groups"
    (is (= (generate-subgroups-naive (int-group. 4)) #{#{0} #{0 2} #{0 1 2 3}}))
    (is (= (generate-subgroups-naive (int-group. 3)) #{#{0} #{0 1 2}}))))

(defn right-coset "Returns the right coset of the subgroup set containing elem"
  [group subgroup-set elem]
  (set (map #(operate group %1 elem) subgroup-set)))

(defn right-cosets "Returns the right cosets of the subgroup set in group"
  [group subgroup-set]
  (set (map
        #(right-coset group subgroup-set %1)
        (elems group))))

(defn left-coset "Returns the left coset of the subgroup set containing elem"
  [group subgroup-set elem]
  (set (map #(operate group elem %1) subgroup-set)))

(defn left-cosets "Returns the left cosets of the subgroup set in group"
  [group subgroup-set]
  (set (map
        #(left-coset group subgroup-set %1)
        (elems group))))

(deftest test-cosets
  (testing "right coset in d4"
    (is (= (right-coset (d4.) (cycle-to-set (d4.) #{:psi}) :psi)
           #{:iota :psi}))
    (is (= (right-coset (d4.) (cycle-to-set (d4.) #{:psi}) :phi)
           #{:phi :theta}))
    (is (= (right-coset (d4.) (cycle-to-set (d4.) #{:psi}) :sigma)
           #{:sigma :mu}))
    (is (= (right-coset (d4.) (cycle-to-set (d4.) #{:psi}) :tau)
           #{:tau :lambda})))
  (testing "right cosets"
    (is (= (right-cosets (d4.) (cycle-to-set (d4.) #{:psi}))
           #{ #{:iota :psi} #{:phi :theta} #{:sigma :mu} #{:tau :lambda}}))))

(defn normal-subgroups [group]
  (filter #(normal? %1 group) (generate-subgroups-naive group)))

(defn order-table "Outputs a table of orders of elements -- WIP"
  [group]
  (let [elements (elems group)
        cycles (map #(cycle-to-set group #{%1}) (elems group))
        counts (map count cycles)]
    (map #(println
           "|" %1 "|" %2 "|" %3 "|") elements cycles counts)))

;; (order-table (direct-product. (int-group. 4) (int-group. 2)))
