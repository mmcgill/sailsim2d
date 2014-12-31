(ns com.mmcgill.sailsim2d.server
  "The server:
   * maintains a game state
   * manages client connections
   * accepts client input
   * broadcasts new game states to all clients"
  (:require [clojure.core.async :as async]
            [clojure.data.json :as json]
            [clojure.set :as set]
            [clojure.stacktrace :as st]

            [com.mmcgill.sailsim2d.model :as m]))

(comment "
The 'game loop' of the server should be independent of
the actual communication mechanism between clients and servers.
We want to support both in-process clients and clients
connected via TCP connections.

In each case, control messages come from the clients to the server,
and game states go from server to clients.

")

(defprotocol ClientManager
  (list-clients [this] "List connected client ids.")
  (read-client-messages [this]
    "Return all client messages received since the last call,
    as a sequence of [id msg] pairs.")
  (broadcast [this msg] "Broadcast message to all connected clients")
  (send-to-client [this id msg] "Send message to client")
  (shutdown [this] "Disconnect all clients and free resources"))

(defprotocol Server
  (send-to-server [this id msg]))

(defprotocol InProcessServer
  (connect [this id handler])
  (disconnect [this id]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; In-processes server
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn pop-n [q n]
  (if (> n 0)
    (recur (pop q) (dec n))
    q))

(defn in-process-client-manager []
  (let [client-map (atom {})
        client-msgs (atom (clojure.lang.PersistentQueue/EMPTY))]
    (reify
      ClientManager
      (list-clients [_] (keys @client-map))
      (read-client-messages [_]
        (let [msgs (seq @client-msgs)]
          (swap! client-msgs pop-n (count msgs))
          msgs))
      (broadcast [_ msg]
        (doseq [ch (vals @client-map)]
          (async/>!! ch msg)))
      (send-to-client [_ id msg]
        (when-let [ch (get @client-map id)]
          (async/>!! ch msg)))
      (shutdown [_]
        (doseq [ch (vals @client-map)]
          (async/close! ch))
        (reset! client-map nil)
        (reset! client-msgs nil))

      Server
      (send-to-server [_ id msg]
        (swap! client-msgs conj [id msg]))

      InProcessServer
      (connect [_ id handler]
        (let [ch (async/chan 10)]
          (async/go-loop []
            (when-let [msg (async/<! ch)]
              (handler msg)
              (recur)))
          (swap! client-map assoc id ch)))
      (disconnect [_ id]
        (when-let [ch (get @client-map id)]
          (async/close! ch)
          (swap! client-map dissoc id))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; In-process test client
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn in-process-echo-client [id]
  (fn [msg] (prn id msg)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Message processing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def message-handlers
  {})

(defn process-message
  [state [id [tag body]]]
  (when-let [handler (get message-handlers tag)]
    (handler state id body)))

(defn process-messages [state msgs]
  (reduce process-message state msgs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Game loop
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn sync-clients [state client-mgr]
  (let [prev-clients (-> state :boats keys set)
        curr-clients (-> client-mgr list-clients set)
        added-clients (set/difference curr-clients prev-clients)
        dropped-clients (set/difference prev-clients curr-clients)]
    (->> added-clients
         (reduce #(assoc %1 %2 (m/boat [0 0])) {})
         (reduce-kv m/add-boat state)
         (#(reduce m/remove-boat % dropped-clients)))))

(defn busy-wait-until [nano-time]
  (loop []
    (when (< (System/nanoTime) nano-time)
      (recur))))

(defn start-server
  "Start a server."
  [initial-state client-mgr]
  (let [stop-ch (async/chan)]
    (async/thread
      (try
        (println "Server started")
        (loop [state initial-state]
          (let [[msg ch] (async/alts!! [stop-ch (async/timeout 0)])
                start-nanos (System/nanoTime)]
            (when (not= ch stop-ch)
              (let [next-state (-> state
                                   (sync-clients client-mgr)
                                   (process-messages (read-client-messages client-mgr))
                                   m/tick)]
                (broadcast client-mgr ["state" next-state])
                ;; TODO: this busy-wait is dumb, we could be processing messages here
                (busy-wait-until (+ start-nanos (long (* m/secs-per-tick 1000000000))))
                (recur next-state)))))
        (catch Throwable ex
          (st/print-cause-trace ex)))
      (shutdown client-mgr)
      (println "Server stopped"))
    stop-ch))

(comment
  (def cm (s/in-process-client-manager))
  (def stop-ch (s/start-server (m/game-state [0 0] [0.5 0.5]) cm))
  )
