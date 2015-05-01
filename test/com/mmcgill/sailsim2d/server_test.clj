(ns com.mmcgill.sailsim2d.server-test
  (:require [clojure.core.async :as async]
            [com.mmcgill.sailsim2d.model :as model]
            [com.mmcgill.sailsim2d.server :as server])
  (:use [clojure.test]))

(deftest test-client-connection-disconnection
  ;; start a server with an in-process client manager
  (let [client-mgr (server/in-process-client-manager)
        events (async/chan)
        event-hdlr (fn [state msg]
                     ;; will block the server until the test is ready
                     ;; to read
                     (async/>!! events msg)
                     state)
        stop-ch (server/start-server (model/game-state [0 0] [0 0]) event-hdlr client-mgr)]
    ;; connect a client
    (let [{:keys [to-client from-client]} (server/connect client-mgr)]
      ;; verify that a connect event was generated
      (let [[_ msg] (async/<!! events)]
        (is (= ["connect" nil] msg)))
      ;; disconnect the client
      (async/close! from-client)
      ;; verify that a disconnect event was generated
      (let [[_ msg] (async/<!! events)]
        (is (= ["disconnect" nil])))
      (async/>!! stop-ch :stop))
    ))
