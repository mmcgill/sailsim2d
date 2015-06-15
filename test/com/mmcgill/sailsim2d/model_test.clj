(ns com.mmcgill.sailsim2d.model-test
  (:require [com.mmcgill.sailsim2d.model :as m]
            [schema.test])
  (:use clojure.test))

(use-fixtures :once schema.test/validate-schemas)

(defn new-client [game-state client-id]
  (-> game-state
      (m/add-incoming client-id ["connect"])))

(defn connected-client [game-state client-id]
  (-> game-state
      (new-client client-id)
      (m/tick)
      (m/pop-all-outgoing)))

(deftest test-connection
  (testing "client connection handshake"
    ;; When a client connects, the server should:
    ;; * create a boat
    ;; * create a ClientMediary
    ;; * send the client its boat id
    (let [client-id "1"
          g (-> (m/game-state [0 0] [0 0])
                (new-client client-id)
                (m/tick))
          msgs (m/peek-all-outgoing g)]
      (is (= 4 (count msgs)))
      (is (= [client-id ["set-boat-id" 0]] (first msgs)))
      (is (m/get-mediary g client-id))
      (is (m/get-entity g 0))
      (is (= [client-id
              ["env-update"
               {:wind [0 0]
                :current [0 0]}]]
             (second msgs)))
      (is (= [client-id
              ["boat-update"
               {:id 0
                :pos [0.0 0.0]
                :v [0.0 0.0]
                :theta 0.0
                :rudder-theta 0.0
                :throttle 0.0}]]
             (nth msgs 2))))))

(defn ticks [game-state n]
  (nth (iterate m/tick game-state) n))

(deftest test-wake-segments
  (testing "wake segments added above wake speed")
  (let [client-id "1"
        g (-> (m/game-state)
              (connected-client client-id)
              (assoc-in [:entities 0 :v] [(+ m/wake-speed 1) 0])
              (ticks m/wake-ticks-per-segment))
        msgs (->> g
                  (m/peek-all-outgoing)
                  (filter #(= "wake-segment" (first (second %1)))))]
    (= 2 (count msgs))))

