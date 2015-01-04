(ns com.mmcgill.sailsim2d.server.websocket
  (:require [org.httpkit.server :as httpkit]
            [clojure.core.async :as async]
            [clojure.data.json :as json]
            [clojure.stacktrace :as st]
            [com.mmcgill.sailsim2d.server :as server]))

(defn encode-msg [msg]
  (json/write-str msg))

(defn decode-msg [msg-str]
  (json/read-str msg-str))

(defn make-websocket-handler [client-map from-clients]
  (fn websocket-handler [request]
    (let [id (str (java.util.UUID/randomUUID))
          to-client (async/chan 3)]
      (httpkit/with-channel request channel
        ;; send client its assigned id as first message
        (httpkit/send! channel (encode-msg ["id" id]))
        (swap! client-map assoc id {:to-client to-client, :websocket channel})
        (httpkit/on-receive channel (fn [msg]
                                      (try
                                        (async/>!! from-clients [id (decode-msg msg)])
                                        (catch Throwable ex
                                          (st/print-cause-trace ex)))))
        (httpkit/on-close   channel (fn [status]
                                      (swap! client-map dissoc id)
                                      (async/close! to-client)))
        (async/go
          (loop []
            (when-let [msg (async/<! to-client)]
              (httpkit/send! channel (encode-msg msg))
              (recur))))))))

(defn read-all-available [ch n]
  (loop [msgs [], i 0]
    (if (= i n)
      msgs
      (let [[msg from-ch] (async/alts!! [ch (async/timeout 0)] :priority true)]
        (if (= ch from-ch)
          (recur (conj msgs msg) (inc i))
          msgs)))))

(defn websocket-manager
  [{:keys [port] :as opts}]
  (let [client-map (atom {})
        client-msgs (async/chan 100)
        stop-fn (httpkit/run-server (make-websocket-handler client-map client-msgs) opts)]
    (println "Started websocket server on" port)
    (reify server/ClientManager
      (list-clients [_] (keys @client-map))
      (read-client-messages [_] (read-all-available client-msgs 100))
      (broadcast [_ msg]
        (doseq [[id {:keys [to-client]}] @client-map]
          (async/>!! to-client msg)))
      (send-to-client [_ id msg]
        (when-let [x (get @client-map id)]
          (async/>!! (:to-client x) msg)))
      (shutdown [this]
        (print "Stopping websocket server on" port "... ")
        ;; stop accepting new connections, give existing clients some time to drop
        (let [f (future (stop-fn :timeout 1000))]
          ;; tell all the clients to disconnect
          (server/broadcast this ["disconnect" nil])
          @f
          (println "stopped")
          (reset! client-map {}))))))
