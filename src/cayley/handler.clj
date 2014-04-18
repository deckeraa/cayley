(ns cayley.handler
  (:use [compojure.core]
        [hiccup.form])
  (:require [compojure.handler :as handler]
            [compojure.route :as route]
            [hiccup.core :as html]
            [cayley.core :as core])
  (:import [cayley.core int-group]
           [cayley.core direct-product]))

(defroutes app-routes
  (GET "/" [] (html/html [:html
                          (group-info (core/direct-product. (core/int-group. 4) (core/int-group. 2)))
]))
  (route/resources "/")
  (route/not-found "Not Found"))

(def app
  (handler/site app-routes))

(defn group-info [group]
  [:div#group-info
   [:div#group-description
    (str (core/group-name group) ": " (core/group-string group))]
   [:div#order-table
    [:p "Order Table"]
    (core/order-table-to-html-hiccup group)]])
