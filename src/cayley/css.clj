(ns cayley.css
  (:require [garden.def :refer [defstylesheet defstyles]]
            [garden.units :refer [px]]))

(def red (garden.color/rgb 250 0 0))
(defstyles screen
  [:body
   {:font-family "sans-serif"
    :font-size (px 16)
    :line-height 1.5}]
  [:.group-info
   {:font-weight :bold}]
  [:.group-description
   {:font-size (px 12)}]
  [:.order-table
   {:border-width :thin
    :border-color :black
    :border-style :inset}])

;; (garden.core/css   [:body
;;    {:font-family "sans-serif"
;;     :font-size (px 16)
;;     :line-height 1.5}]
;;   [:.group-info
;;    {:color :red}])

;; .group-info {
;;     color: red;
;; }

;; .group-description {
;;     color: red;
;; }

;; .order-table {
;;     border: 4px dashed black;
;; }
