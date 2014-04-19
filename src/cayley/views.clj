(ns cayley.views
  (:use [clojure.test :as test])
  (:require [cayley.core :as core])
  (:import [cayley.core int-group]
           [cayley.core direct-product]
           [cayley.core d4]))

(defn order-table "Creats a table of group elements: [element cycle order]"
  [group]
  (let [elements (core/elems group)
        cycles (map #(core/cycle-to-set group #{%1}) (core/elems group))
        counts (map count cycles)]
    (cons ["Element" "Cycle" "Order"]
            (map #(vector %1 %2 %3)
                 elements cycles counts))))

(deftest test-order-table
  (testing "order table of Z_4"
    (is (= (order-table (core/int-group. 4))
           [["Element" "Cycle" "Order"]
            [0 	#{0} 	   1]
            [1 	#{0 1 2 3} 4]
            [2 	#{0 2} 	   2]
            [3 	#{0 1 2 3} 4]]))))

(defn print-shim "A collection of tweaks and hacks to output things nicely."
  [obj]
  nil)

(defn table-to-org-table "Takes a 2-deep vector and turns it into an org-table"
  [table]
  (let [prt-tbl (map #(map pr-str %1) (rest t)) ; handle caption
             full-tbl (cons (first t) prt-tbl)]
         (clojure.string/join "\n"
                              (map
                               #(clojure.string/join (interpose "|" %1))
                               full-tbl))))

(deftest test-table-to-org-table
  (testing "order table of Z_4"
    (is (= (table-to-org-table (order-table (int-group. 4)))
           "Element|Cycle|Order\n0|#{0}|1\n1|#{0 1 2 3}|4\n2|#{0 2}|2\n3|#{0 1 2 3}|4"))))

(->> t
     (map pr-str))

(print (let [prt-tbl (map #(map pr-str %1) (rest t)) ; handle caption
             full-tbl (cons (first t) prt-tbl)]
         (clojure.string/join "\n"
                              (map
                               #(clojure.string/join (interpose "|" %1))
                               full-tbl))))

(print (clojure.string/join "\n" '("a" "b" "c")))

(def t [["Element" "Cycle" "Order"]
            [0 	#{0} 	   1]
            [1 	#{0 1 2 3} 4]
            [2 	#{0 2} 	   2]
            [3 	#{0 1 2 3} 4]])

(defn order-table-to-html-hiccup
  "Outputs an html table (hiccup) of orders of elements"
  [group]
  (let [elements (core/elems group)
        cycles (map #(core/cycle-to-set group #{%1}) (core/elems group))
        counts (map count cycles)]
    [:table
     [:tr [:td "Element"] [:td "Cycle"] [:td "Order"]]
     (map #(vector
            :tr
            [:td (prn-str %1)]
            [:td (prn-str %2)]
            [:td (prn-str %3)]) elements cycles counts)]))

(defn group-info [group]
  [:div.group-info
   [:div.group-description
    (str (core/group-name group) ": " (core/group-string group))]
   [:div.order-table
    [:p "Order Table"]
    (core/order-table-to-html-hiccup group)]])
