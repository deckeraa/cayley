(ns cayley.handler
  (:use [compojure.core]
        [hiccup.form])
  (:require [compojure.handler :as handler]
            [compojure.route :as route]
            [hiccup.core :as html]
            [cayley.core :as core]
            [garden.core :as css]
            [garden.units :as u :refer [px pt]])
  (:import [cayley.core int-group]
           [cayley.core direct-product]))

(defn group-info [group]
  [:div.group-info
   [:div.group-description
    (str (core/group-name group) ": " (core/group-string group))]
   [:div.order-table
    [:p "Order Table"]
    (core/order-table-to-html-hiccup group)]])

(defroutes app-routes
  (GET "/" [] (html/html [:html
                          [:link {:rel "stylesheet" :type "text/css" :href "/css/group.css"}]
                          (group-info (core/direct-product. (core/int-group. 4) (core/int-group. 2)))
                          ]))
  (route/resources "/" {:root "."} )
  (route/not-found "Not Found"))

(def app
  (handler/site app-routes))
