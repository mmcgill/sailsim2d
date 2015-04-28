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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; In-processes server
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol InProcessServer
  (connect [this] "Returns {:from-client <chan> :to-client <chan>}"))

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
        (doseq [{:keys [out-ch]} (vals @client-map)]
          (async/>!! out-ch msg)))
      (send-to-client [_ id msg]
        (when-let [{:keys [out-ch]} (get @client-map id)]
          (async/>!! out-ch msg)))
      (shutdown [_]
        (doseq [ch (vals @client-map)]
          (async/close! ch))
        (reset! client-map nil)
        (reset! client-msgs nil))

      InProcessServer
      (connect [_]
        ;; TODO: Magic constants!
        (let [id (str (java.util.UUID/randomUUID))
              in-ch (async/chan (async/dropping-buffer 100))
              out-ch (async/chan (async/dropping-buffer 100))]
          (async/go-loop []
            (if-let [msg (async/<! in-ch)]
              (do
                (swap! client-msgs conj [id msg])
                (recur))
              (do
                (async/close! in-ch)
                (async/close! out-ch)
                (swap! client-map dissoc id)
                (swap! client-msgs conj [id ["disconnect" nil]]))))
          (swap! client-map assoc id {:in-ch in-ch :out-ch out-ch})
          (swap! client-msgs conj [id ["connect" nil]])
          {:from-client in-ch :to-client out-ch})))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Message processing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def message-handlers
  {"set-rudder-theta"
   (fn [state id theta]
     (when (get-in state [:boats id])
       (let [theta (cond
                     (> theta (/ Math/PI 4)) (/ Math/PI 4)
                     (< theta (/ Math/PI -4)) (/ Math/PI -4)
                     :else theta)]
         (assoc-in state [:boats id :rudder-theta] theta))))})

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

(defn wait-until [nano-time]
  (let [millis (/ (- nano-time (System/nanoTime)) 1000000.0)]
    (when (> millis 0)
      (Thread/sleep millis)))
  #_(loop []
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
                (wait-until (+ start-nanos (long (* m/secs-per-tick 1000000000))))
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
