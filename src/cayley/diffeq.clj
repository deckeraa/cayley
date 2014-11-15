(ns cayley.diffeq
  (:use ;; [clojure.string :as string]
        [clojure.test :as test]
        [clojure.math.numeric-tower :as math]))

;;; This files contains some ideas for a symbolic calculus system that I
;;; want to write as I do through "Differential Equations with Modeling Applications 9th edition" (Dennis Zill).
;;; The below code isn't intended to be well-designed or even to necessarily
;;; work.

(def '^ 'math/expt)

(defn power-rule-naive
  "(math/expt x 3) -> (* 3 (math/expt x 2)"
  [expr]
  (let [our-fn   (first expr)
        our-var  (second expr)
        our-expt (nth expr 2)]
    (if (= our-expt 0)
      1
      (list '* our-expt (list 'math/expt our-var (- our-expt 1))))))

(power-rule-naive '(* x 0))
(power-rule-naive '(* x 3))


(let [x 3]
   (power-rule-naive '(* x 3)))


