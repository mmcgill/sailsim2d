(ns com.mmcgill.sailsim2d.http
  "Built-in HTTP server for serving the browser client's static content."
  (:require [compojure.core :refer :all]
            [compojure.route :as route]
            [hiccup.core :as hiccup]
            [org.httpkit.server :as httpkit]))

(defn index [req]
  (hiccup/html
   [:html
    [:head
     [:script {:src "/static/sailsim2d.js"}]
     [:title "SailSim 2D"]]
    [:body
     [:h1 "SailSim 2D"]
     [:div {:style "display:none;"}
      [:img {:id "boat" :src "/static/boat.png"}]]
     [:canvas {:id "sailsim_canvas" :width "640" :height "480" :style "border:1px dotted;"}]]]))

(defroutes app
  (GET "/"           [] index)
  (GET "/index.htm"  [] index)
  (GET "/index.html" [] index)
  (route/files "/static/" {:root "public"})
  (route/resources "/static/" {:root "public"})
  (GET "/*"          [] (fn [req] (prn req)))
  (route/not-found "<h1>Page not found</h1>"))

(defn start [{:keys [port] :as opts}]
  (httpkit/run-server #'app opts))

