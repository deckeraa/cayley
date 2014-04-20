(ns cayley.handler
  (:use [compojure.core]
        [hiccup.form])
  (:require [compojure.handler :as handler]
            [compojure.route :as route]
            [hiccup.core  :as html]
            [cayley.core  :as core]
            [cayley.views :as views]
            [garden.core :as css]
            [garden.units :as u :refer [px pt]])
  (:import [cayley.core int-group]
           [cayley.core direct-product]
           [cayley.core d4]))

(defroutes app-routes
  (GET "/" [] (html/html [:html
                          [:link {:rel "stylesheet" :type "text/css" :href "/css/group.css"}]
                          (views/group-info (core/parse "Z_4"))
                          (views/group-info (core/parse "Z_2 x Z_4"))
                          (views/group-info (core/parse "Z_2 x Z_2 x Z_2"))
                          (views/group-info (core/d4. ))
                          ]))
  (route/resources "/" {:root "."} )
  (route/not-found "Not Found"))

(def app
  (handler/site app-routes))
