(ns cayley.diffeq
  (:use [clojure.string :as str]
        [clojure.test :as test]
        [clojure.math.numeric-tower :as math]
        [clojure.walk :as walk]
        [instaparse.core :as insta]))

;;; This files contains some ideas for a symbolic calculus system that I
;;; want to write as I go through "Differential Equations with Modeling Applications 9th edition" (Dennis Zill).
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
    (if (= our-expt 0)
      1
      (list '* our-expt (list 'math/expt our-var (- our-expt 1))))))

(power-rule-naive '(math/expt x 0))
(power-rule-naive '(math/expt x 3))


;; (let [x 3]
;;    (power-rule-naive '(* x 3)))

;; (eval (let [x 3] (list '* 'x 2)))
;; (let [x 3] '(* x 2))
;; (let [x 3]
;;   (= (list '* 'x 2)
;;      (list '*  x 2)))

;; (let [x 3] '(* x 2))

;; (walk/postwalk 
;;  (fn [e] (if (string? e) (keyword e) e)) 
;;  {"hello" ["world" {:foo '(bar "baz")}]})



;; (eval (d-subs 'x 2 '(* x 3)))

;;; work on reading infix notation -- wip
(def expr-str "2+5")
(defn read-infix 
  "Translates an infix mathematical expression in a string into an s-expression that can be eval'd"
  [expr-str]
  (let [terms (split expr-str #"\+" 0)]
    (if (= 1 (count terms)) ;; individual term
      (read-string (first terms))
      (cons '+ (map read-infix terms))
      )))

(deftest test-read-infix
  (testing "simple-addition"
    (is (= (read-infix "2+3+5") '(+ 2 3 5)))
   )
  (testing "parenthesis"
    (is (= (eval (read-infix "2+(3-5)")) 0))
    ))

;; (eval (read-infix "2+3+5"))

;; (str/split "1+(2+3)" #"[\([^\)]*\)]*\+" )
;; (re-seq #"\+" "1+(2+3)")
;; (str/split "()1+2" #"[\(\)]*(\+)")
;; (str/split "(1+2)+3" #"[\([^\)]*\)]*\+")
;; (re-seq  #"[\([^\)]*\)]*\+" "(1+2)+3")
;; (re-seq  #"[\(\)]*\+" "()+3")


;; This grammer is incomplete
;; TODO:
;;  1) handle arbitrary variable names
;;  2) handle arbitrary function names
;;  3) This might require generating a new parser at run time, based
;;     on the variables and functions the user is using.
(def infix-naive
  (insta/parser
   "S = E
    E = E '+' E 
      | E '-' E
      | 'x'"
   ))

(infix-naive "x+x-x")

;; TODO figure out how to transfrom parser output into s-exp
(walk/postwalk
 (fn [e] (if (= [:E "x"] e) 'x e))
 (infix-naive "x+x"))
