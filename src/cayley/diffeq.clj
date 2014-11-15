(ns cayley.diffeq
  (:use ;; [clojure.string :as string]
        [clojure.test :as test]
        [clojure.math.numeric-tower :as math]
        [clojure.walk :as walk]))

;;; This files contains some ideas for a symbolic calculus system that I
;;; want to write as I do through "Differential Equations with Modeling Applications 9th edition" (Dennis Zill).
;;; The below code isn't intended to be well-designed or even to necessarily
;;; work.

(defn d-subs [sym val expr]
  "Substitutes a value for a symbol in an expr
   example: (d-subs 'x 2 '(+ x 3)) -> '(+ 2 3)
   example: (d-subs '(+ x 3) 'u '(* u (+ x 3))) -> '(* u u)))"
  (walk/postwalk
   (fn [e] (if (= sym e) val e))
   expr))

(deftest test-d-subs
  (testing "d-subs"
    (is (= (d-subs 'x 2 '(+ x 3)) '(+ 2 3)))
    (is (= (d-subs '(+ x 3) 'u '(* u (+ x 3))) '(* u u)))
   ))

(defn power-rule-naive
  "(math/expt x 3) -> (* 3 (math/expt x 2)"
  [expr]
  (let [our-fn   (first expr)
        our-var  (second expr)
        our-expt (nth expr 2)]
    (println (type our-expt) )
    (if (= our-expt 0)
      1
      (list '* our-expt (list 'math/expt our-var (- our-expt 1))))))

(power-rule-naive '(math/expt x 0))
(power-rule-naive '(math/expt x 3))


(let [x 3]
   (power-rule-naive '(* x 3)))

(eval (let [x 3] (list '* 'x 2)))
(let [x 3] '(* x 2))
(let [x 3]
  (= (list '* 'x 2)
     (list '*  x 2)))

(let [x 3] '(* x 2))

(walk/postwalk 
 (fn [e] (if (string? e) (keyword e) e)) 
 {"hello" ["world" {:foo '(bar "baz")}]})



(eval (d-subs 'x 2 '(* x 3)))
