(ns user
  (:require [clojure.core.async :as async]
            [com.mmcgill.sailsim2d.model :as m]
            [com.mmcgill.sailsim2d.server :as s]
            [com.mmcgill.sailsim2d.server.websocket :as ws]
            [com.mmcgill.sailsim2d.http :as http]))

(defonce system (atom nil))

(defn stop []
  (when @system
    (let [{:keys [stop-ch stop-fn mgr]} @system]
      (async/close! stop-ch)
      (stop-fn)
      (s/shutdown mgr))
    (reset! system nil)))

(defn go-ipc []
  (stop)
  (let [mgr (s/in-process-client-manager)]
    (try
      (reset! system {:stop-ch (s/start-server (m/game-state [0 0] [0.3 0.2]) mgr)
                      :stop-fn (constantly nil)
                      :mgr mgr})
      (catch Throwable ex
        (s/shutdown mgr)
        (throw ex)))))

(defn go [& {:keys [websocket-port http-port]
             :or {websocket-port 9090, http-port 8080}}]
  (stop)
  (let [mgr (ws/websocket-manager {:port websocket-port})
        width (* 5 (Math/sqrt 2))
        g (-> (m/game-state [6 3] [0.3 0.2])
              (m/add-entity
               (m/course
                (m/circular-course-spec [0 0] 50 10 15)))
              first)]
    (try
      (reset! system {:stop-ch (s/start-server g mgr)
                      :stop-fn (http/start {:port http-port})
                      :mgr mgr})
      (catch Throwable ex
        (s/shutdown mgr)
        (throw ex)))))
