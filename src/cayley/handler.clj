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
                          (core/order-table-to-html-hiccup (core/direct-product. (core/int-group. 4) (core/int-group. 2)))
                          (core/order-table-to-html-hiccup (core/int-group. 4))]))
  (route/resources "/")
  (route/not-found "Not Found"))

(def app
  (handler/site app-routes))
