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

(defn d-subs 
 ([sym val expr]
  "Substitutes a value for a symbol in an expr
   example: (d-subs 'x 2 '(+ x 3)) -> '(+ 2 3)
   example: (d-subs '(+ x 3) 'u '(* u (+ x 3))) -> '(* u u)))"
  (walk/postwalk
   (fn [e] (if (= sym e) val e))
   expr))
 ([sym-val-pair-list expr]
    (let [var-hash (apply hash-map sym-val-pair-list)]
      (walk/postwalk
       (fn [e] (if (var-hash e) (var-hash e) e))
       expr))))

(deftest test-d-subs
  (testing "d-subs single variable"
    (is (= (d-subs 'x 2 '(+ x 3)) '(+ 2 3)))
    (is (= (d-subs '(+ x 3) 'u '(* u (+ x 3))) '(* u u)))
   )
  (testing "d-subs multi-variable"
    (is (= (d-subs ['x 1 'y 2] '(+ x y)) '(+ 1 2))))
  )

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
      | E infix-operation E
      | variable
    <variable> = #'[A-Za-z]'
    <infix-operation> = '+' | '-' | '*' | '/' "
   ))

(infix-naive "x+x*y")

;; TODO figure out how to transfrom parser output into s-exp
(def expr (infix-naive "x+x"))
(defn insta_to_sexpr [expr]
  (->> 
   expr
   (walk/postwalk ;; get rid of the terminal markers
    (fn [e] (if (and 
                 (vector? e)
                 (= 2 (count e))
                 (= :E (e 0))) (e 1) e)))
   (walk/postwalk ;; transform the infix [:E "x" "+" "x"]-type things to (+ x x)
    (fn [e] 
      (if (and (vector? e) (= :E (first e))) (map #(if (string? %1) (symbol %1) %1) (list (e 2) (e 1) (e 3))) e))
    )
   (walk/postwalk ;; take care of the "S" block -- should be something like [:S (+ x x)]
    (fn [e]
      (if (and (vector? e) (= :S (e 0))) (e 1) e)))))

(deftest test-insta_to_sexpr
  (testing "insta_to_sexpr"
    (is (= (insta_to_sexpr (infix-naive "x+x")) '(+ x x)))
    (is (= (eval (d-subs 'x 2 (insta_to_sexpr (infix-naive "x+x+x")))) 6))
    (is (= (eval (d-subs 'y 3 (insta_to_sexpr (infix-naive "y+y+y")))) 9))
    (is (= (eval (d-subs ['x 1 'y 2 'z 3] (insta_to_sexpr (infix-naive "x+y*z")))) 7))
   ))
