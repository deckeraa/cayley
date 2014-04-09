(ns cayley.core)

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

;; (defn star [table elem1 elem2]
;;   (get (get table elem1) elem2))

;; Operate elements together. First argument is the Cayley table as above.
(def star
  (fn this
    ([table elem1 elem2]
       (get (get table elem1) elem2))
    ([table elem1 elem2 & more-elems]
       (apply this table (this table elem1 elem2) more-elems))))

(defn inverse [table elem]
  (:iota (clojure.set/map-invert (get table elem))))

(defn conjugate "Conjugate elem1 with elem2: elem2*elem1*elem2^{-1}" [table elem1 elem2]
  (star table elem2 elem1 (inverse table elem2)))

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
      (new-cycle-to-set table newly-found))))

;; Evaluate these for 21.22
(normal? d4-cayley (cycle-to-set d4-cayley #{:iota}) d4-elems)
(normal? d4-cayley (cycle-to-set d4-cayley #{:theta}) d4-elems)
(normal? d4-cayley (cycle-to-set d4-cayley #{:psi}) d4-elems)
(normal? d4-cayley (cycle-to-set d4-cayley #{:phi}) d4-elems)
(normal? d4-cayley (cycle-to-set d4-cayley #{:lambda}) d4-elems)
(normal? d4-cayley (cycle-to-set d4-cayley #{:mu}) d4-elems)
(normal? d4-cayley (cycle-to-set d4-cayley #{:theta :psi}) d4-elems)
(normal? d4-cayley (cycle-to-set d4-cayley #{:tau}) d4-elems)
(normal? d4-cayley (cycle-to-set d4-cayley #{:lambda :mu}) d4-elems)
(normal? d4-cayley (cycle-to-set d4-cayley d4-elems) d4-elems)

;; playground -- these should really get turned into unit tests somehow
(map #(list (str :theta " conj " %1) (conjugate d4-cayley :theta %1)) d4-elems)
(gen-single-conjugation-map d4-cayley :theta d4-elems)
(gen-conjugation-maps d4-cayley #{:theta :psi} d4-elems)
(normal? d4-cayley #{:theta} d4-elems)
(normal? d4-cayley (cycle-to-set d4-cayley :theta) d4-elems)
(conjugate d4-cayley :psi :lambda)
(contains? #{:theta} :theta)
(star d4-cayley :psi :lambda)
(star d4-cayley :lambda :tau)
(star d4-cayley :sigma :theta :tau)
(star d4-cayley :lambda :psi :lambda)
(conjugate d4-cayley :theta :tau)
(star d4-cayley :tau :theta :sigma)
((partial star d4-cayley) :tau :lambda)

(inverse d4-cayley :tau)

(take-while #(not= :iota %1) (rest (iterate #(star d4-cayley %1 :theta) :iota)))
(cycle-to-set d4-cayley :tau)

(clojure.set/union (cycle-to-set d4-cayley :theta) (cycle-to-set d4-cayley :tau))
(for [x #{:tau :sigma}
      y #{:iota :phi}]
  (star d4-cayley x y))
(operate-many d4-cayley (operate-many d4-cayley #{:tau :phi}))
(cycle-to-set d4-cayley #{:lambda :mu})
