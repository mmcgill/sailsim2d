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
     [:style {:media "screen" :type "text/css"}
      ;; Having to jump to strings here is ugly
      "html, body {"
      "  width: 90%;"
      "  height: 90%;"
      "  margin: 0px;"
      "}"]
     [:title "SailSim 2D"]]
    [:body
     [:div {:style "display:none;"}
      [:img {:id "boat" :src "/static/boat.png"}]]
     [:canvas {:id "sailsim_canvas" #_#_:style "border:1px dotted;"}]]]))

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

