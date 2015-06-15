(ns com.mmcgill.sailsim2d.server-test
  (:require [clojure.core.async :as async]
            [com.mmcgill.sailsim2d.model :as model]
            [com.mmcgill.sailsim2d.server :as server])
  (:use [clojure.test]))

(use-fixtures :once schema.test/validate-schemas)

(deftest test-client-connection-disconnection
  ;; start a server with an in-process client manager
  (let [client-mgr (server/in-process-client-manager)
        stop-ch (server/start-server (model/game-state [0 0] [0 0]) client-mgr)]
    (try
      ;; connect a client
      (let [{:keys [to-client from-client]} (server/connect client-mgr)]
        ;; first message should be the client id
        (let [[msg ch] (async/alts!! [to-client (async/timeout 100)])]
          (is (= "id" (first msg))))
        ;; second message should be the boat id
        (let [[msg ch] (async/alts!! [to-client (async/timeout 100)])]
          (is (= "set-boat-id" (first msg)))))
      (finally
        (async/>!! stop-ch :stop)))))
