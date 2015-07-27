(ns com.mmcgill.sailsim2d.model
  "The sailing model and physics calculations."
  (:require [clojure.pprint :refer [pprint]]
            [schema.core :as s]))

;;;;;; Constants ;;;;;;;;;;;;;;;;;;;;;;;;;

(def ticks-per-sec 30)
(def secs-per-tick (/ 1.0 ticks-per-sec))
(def water-rho 1000.0) ; kg/m^3
(def air-rho 1.225)    ; kg/m^3
(def wake-ttl 200)     ; ticks
(def wake-speed 1.0)   ; m/s
(def wake-ticks-per-segment 10)
(def motor-accel 2.0)  ; m/s^2

;;;;;; Game state and ticks ;;;;;;;;;;;;;;

(s/defschema Vec [(s/one s/Num "x") (s/one s/Num "y")])

(s/defschema Id s/Int)

(defprotocol Entity
  (tick-entity- [entity id game-state]))

(defn Msg
  [tag & payload]
  (apply vector (s/one (s/eq tag) "tag") payload))

(s/defschema IncomingMsg
  (s/either
   (Msg "connect")
   (Msg "disconnect")
   (Msg "set-rudder-theta" s/Num)
   (Msg "set-throttle"     s/Num)))

(s/defschema BoatUpdate
  {:id Id
   :pos Vec
   :v Vec
   :theta s/Num
   :rudder-theta s/Num
   :throttle s/Num
   :length s/Num})

(s/defschema EnvironmentUpdate
  {:wind Vec
   :current Vec})

(s/defschema WakeSegment
  {:id Id
   :pos Vec
   :v Vec
   :ttl s/Int
   (s/optional-key :head-id) Id})

(s/defschema OutgoingMsg
  (s/either
   (Msg "set-boat-id" s/Num)
   (Msg "env-update" EnvironmentUpdate)
   (Msg "boat-update" BoatUpdate)
   (Msg "wake-segment" WakeSegment)
   (Msg "tick" s/Int)))

(defprotocol ClientMediary
  "The server-side representative of a connected client,
   responsible for updating game state in response to
   messages from the client."
  (handle-message- [this game-state msg]))

(s/defschema GameState
  {:t       s/Int
   :wind    Vec
   :current Vec
   :entities {Id (s/protocol Entity)}
   :mediaries {s/Str (s/protocol ClientMediary)}
   :outbox (s/queue (s/pair s/Str "client-id" OutgoingMsg "msg"))
   :inbox (s/queue (s/pair s/Str "client-id" IncomingMsg "msg"))
   :next-id Id})

(s/defn tick-entity :- GameState
  [game-state :- GameState, id :- Id, entity :- (s/protocol Entity)]
  (tick-entity- entity id game-state))

(s/defn game-state :- GameState
  "Create a new game state."
  ([] (game-state [0 0] [0 0]))
  ([wind :- Vec, current :- Vec]
   {:t       0              ; tick number
    :wind    wind           ; [x y]  meters/sec
    :current current        ; [x y]  meters/sec
    :entities {}            ; map of id -> object
    :mediaries {}
    :outbox (s/as-queue [])
    :inbox (s/as-queue [])
    :next-id 0}))

(s/defn fresh-id :- [(s/one GameState "state") (s/one Id "id")]
  [game-state]
  [(update-in game-state [:next-id] inc) (:next-id game-state)])

(s/defn add-entity :- [(s/one GameState "state") (s/one Id "id")]

  ([game-state :- GameState
    entity :- (s/protocol Entity)]
   (let [id (:next-id game-state)]
     (add-entity (update-in game-state [:next-id] inc) entity id)))

  ([game-state :- GameState
    entity :- (s/protocol Entity)
    id :- Id]
   (when (contains? (:entities game-state) id)
     (throw (ex-info (str "Duplicate ID " id)
                     {:entities [entity (get-in game-state [:entities id])]})))
   [(update-in game-state [:entities] assoc id entity)
    id]))

(s/defn get-entity :- (s/maybe (s/protocol Entity))
  [game-state :- GameState, id :- Id]
  (get-in game-state [:entities id]))

(s/defn update-entity :- GameState
  [game-state :- GameState, id :- Id, entity :- (s/protocol Entity)]
  (update-in game-state [:entities] assoc id entity))

(s/defn remove-entity :- GameState
  [game-state :- GameState, id :- Id]
  (update-in game-state [:entities] dissoc id))

(s/defn add-mediary :- GameState
  [game-state :- GameState
   id :- s/Str
   mediary :- (s/protocol ClientMediary)]
  (update-in game-state [:mediaries] assoc id mediary))

(s/defn get-mediary :- (s/maybe (s/protocol ClientMediary))
  [game-state :- GameState, id :- s/Str]
  (get-in game-state [:mediaries id]))

(s/defn remove-mediary :- GameState
  [game-state :- GameState, id :- s/Str]
  (update-in game-state [:mediaries] dissoc id))

;;;;;; Messaging functions ;;;;;;;;;;;;;

(s/defn add-outgoing :- GameState
  [game-state :- GameState
   client-id :- s/Str
   msg :- OutgoingMsg]
  (update-in game-state [:outbox] conj [client-id msg]))

(s/defn broadcast-outgoing :- GameState
  [game-state :- GameState
   msg :- OutgoingMsg]
  (reduce #(add-outgoing %1 %2 msg)
          game-state
          (keys (:mediaries game-state))))

(s/defn broadcast-all-outgoing :- GameState
  [game-state :- GameState
   msgs :- [OutgoingMsg]]
  (reduce broadcast-outgoing game-state msgs))

(s/defn peek-all-outgoing :- (s/queue (s/pair s/Str "client-id" OutgoingMsg "msg"))
  [game-state :- GameState]
  (:outbox game-state))

(s/defn pop-all-outgoing :- GameState
  [game-state :- GameState]
  (assoc-in game-state [:outbox] (s/as-queue [])))

(s/defn add-incoming :- GameState
  [game-state :- GameState
   client-id :- s/Str
   msg :- IncomingMsg]
  (update-in game-state [:inbox] conj [client-id msg]))

(s/defn add-all-incoming :- GameState
  [game-state :- GameState
   msgs :- [(s/pair s/Str "client-id" IncomingMsg "msg")]]
  (reduce #(add-incoming %1 (first %2) (second %2)) game-state msgs))

(s/defn broadcast-incoming :- GameState
  [game-state :- GameState
   msg :- IncomingMsg]
  (reduce #(add-incoming %1 %2 msg)
          game-state
          (keys (:mediaries game-state))))

(s/defn broadcast-all-incoming :- GameState
  [game-state :- GameState
   msgs :- [IncomingMsg]]
  (reduce broadcast-incoming game-state msgs))

(s/defn peek-all-incoming :- (s/queue (s/pair s/Str "client-id" IncomingMsg "msg"))
  [game-state :- GameState]
  (:inbox game-state))

(s/defn pop-all-incoming :- GameState
  [game-state :- GameState]
  (assoc-in game-state [:inbox] (s/as-queue [])))

;;;;;; Boats ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn vadd [[x1 y1] [x2 y2]] [(+ x1 x2) (+ y1 y2)])
(defn vsub [[x1 y1] [x2 y2]] [(- x1 x2) (- y1 y2)])
(defn vmul [[x1 y1] [x2 y2]] [(* x1 x2) (* y1 y2)])
(defn vdiv [[x1 y1] [x2 y2]] [(/ x1 x2) (/ y1 y2)])
(defn vrot [[x y] theta]
  (let [a (Math/sin theta), b (Math/cos theta)]
    [(- (* x b) (* y a)), (+ (* x a) (* y b))]))
(defn vangle [[x y]] (Math/atan2 y x))
(defn todeg [rad] (Math/toDegrees rad))
(defn vmag [[x y]] (Math/sqrt (+ (* x x) (* y y))))
(defn vnorm [[x y :as v]]
  (let [n (vmag v)]
    [(/ x n) (/ y n)]))
(defn vdot [[x1 y1] [x2 y2]]
  (+ (* x1 x2) (* y1 y2)))

(def rudder-coefficient "Scales the rudder torque"      10#_1.5)
(def rotation-damping   "Damps rotation of the boat"    1.5)
(def linear-damping     "Damps velocity of the boat"    0.5)

(defn compute-drag
  "Compute force of drag as a vector: (* (1/2) rho v v (cd theta)),
   where theta is the angle of v.

   rho:   mass density of fluid
   v:     relative velocity of fluid as a vector
   theta: orientation of the solid body
   cda:   [theta -> drag coefficient * area]"
  [rho [x y] theta cda]
  (let [fluid-theta (Math/atan2 y x)
        fmag (* 0.5 rho (cda (- fluid-theta theta)) (+ (* x x) (* y y)))]
    (vrot [fmag 0] fluid-theta)))

(defn hull-cda [theta]
  ;; TODO: Use interpolation and pre-calculated table for hull shape
  (let [k (Math/cos (+ (* 2 theta) Math/PI))]
    (/ (+ 1.5 (* k (+ 2.2 (* 0.8 k)))) 6.0)))

(defn sail-cda [theta]
  ;; TODO: Use interpolation and pre-calculated table for sail shape
  (* 4 (hull-cda theta)))

(defn rudder-cda [theta]
  (* 0.0625 (/ (+ (Math/cos (+ (* 2 theta) Math/PI)) 1) 2)))

(defn moment-of-inertia
  "Moment of inertia of a rod of length l and mass m, with COM at rod's center.
  http://en.wikipedia.org/wiki/List_of_moments_of_inertia"
  [l m]
  (/ (* m l l) 12))

(defn rudder-torque [r current rudder-theta]
  (let [theta (- rudder-theta (vangle current))
        speed (vmag current)]
    (* r rudder-coefficient speed speed (+ (Math/sin theta) #_(Math/sin (* 3 theta))))))

(s/defn fresh-wake-ids [game-state]
  (let [[game-state lwake-id] (fresh-id game-state)
        [game-state rwake-id] (fresh-id game-state)]
    [game-state lwake-id rwake-id]))

(s/defn wake-segments :- [(s/one GameState "state")
                          (s/one (s/maybe Id) "lwake-id")
                          (s/one (s/maybe Id) "rwake-id")]
  [game-state id pos v theta lwake-id rwake-id]
  (if (> (vmag v) wake-speed)
    (let [[game-state lwake-id rwake-id]
          (if (nil? lwake-id)
            (fresh-wake-ids game-state)
            [game-state lwake-id rwake-id])]
      (if (= (mod (:t game-state) wake-ticks-per-segment) 0)
        (let [p (vadd pos (vrot [-1.8,0] theta))]
          [(-> game-state
               (broadcast-outgoing ["wake-segment"
                                    {:id lwake-id
                                     :pos p
                                     :v (vrot v (- (/ (* 2 Math/PI) 3)))
                                     :ttl wake-ttl
                                     :head-id id}])
               (broadcast-outgoing ["wake-segment"
                                    {:id rwake-id
                                     :pos p
                                     :v (vrot v (/ (* 2 Math/PI) 3))
                                     :ttl wake-ttl
                                     :head-id id}]))
           lwake-id
           rwake-id])
        [game-state lwake-id rwake-id]))
    [game-state nil nil]))

(defrecord Boat [sail-theta rudder-theta pos v a theta
                 alpha omega mass length throttle
                 lwake-id rwake-id]
  Entity
  (tick-entity- [_ id game-state]
    ;; TODO: compute net force and torque, then apply to
    ;; velocity and position
    (let [current (vsub (:current game-state) v)
          tr (rudder-torque (/ length 2) current (+ theta rudder-theta))
          td (* (- rotation-damping) omega (/ length 2))
          t (+ tr td)
          fh (compute-drag water-rho current theta hull-cda)
          fm (vrot [(* motor-accel mass throttle) 0] theta)        ; TEMP: motor
          ;;fm [0 0]
          fd (vsub [0 0] (vmul v [linear-damping linear-damping]))
          f (vadd (vadd fh fd) fm)

          ;; rotational dynamics
          alpha (/ t (moment-of-inertia length mass))
          omega (+ omega (* secs-per-tick alpha))
          theta (+ theta (* secs-per-tick omega))

          ;; linear dynamics
          a (vdiv f [mass mass])
          v (vadd v (vmul a [secs-per-tick secs-per-tick]))
          pos (vadd pos (vmul v [secs-per-tick secs-per-tick]))

          ;; wake
          [game-state lwake-id rwake-id] (wake-segments game-state id pos v theta lwake-id rwake-id)
          ]
      (-> game-state
          (broadcast-outgoing
           ["boat-update"
            {:id id
             :pos pos
             :v v
             :theta theta
             :rudder-theta rudder-theta
             :throttle throttle
             :length length}])
          (update-entity
           id
           (Boat. sail-theta rudder-theta pos v a theta
                  alpha omega mass length throttle
                  lwake-id rwake-id))))))

(s/defschema SBoat
  (s/both Boat
          {:sail-theta   s/Num
           :rudder-theta s/Num
           :pos          Vec
           :v            Vec
           :a            Vec
           :theta        s/Num
           :alpha        s/Num
           :omega        s/Num
           :mass         s/Num
           :length       s/Num
           :throttle     s/Num
           :lwake-id     (s/maybe Id)
           :rwake-id     (s/maybe Id)}))

(s/defn boat :- SBoat
  [pos :- Vec]
  (Boat.
   0.0         ; angle, radians
   0.0         ; angle, radians
   pos         ; vector
   [0.0 0.0]   ; vector
   [0.0 0.0]   ; vector
   0.0         ; angle, radians
   0.0         ; radians/sec^2
   0.0         ; raians/sec
   56.0        ; kg
   3.6         ; m
   0.0         ; 0-1
   nil
   nil))

(defn clamp [low high value]
  (max low (min high value)))

(defn set-field [state id field value]
  (if-let [entity (get-entity state id)]
    (update-entity state id (assoc entity field value))
    (do (println "No such entity " id)
        state)))


(defrecord PlayerClientMediary [boat-id]
  ClientMediary
  (handle-message- [_ game-state [tag body]]
    (case tag
      "set-rudder-theta"
      (set-field game-state boat-id :rudder-theta
                 (clamp (/ Math/PI -4) (/ Math/PI 4) body))

      "set-throttle"
      (set-field game-state boat-id :throttle
                 (clamp 0 1 body))

      (do (println "Unknown message tag " tag)
          game-state))))

;;;;;; Course ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A CourseSpec is a sequence of [point,width] pairs, each
;; pair representing the center point of a pair of bouys and
;; their distance from one another. A [point,width] pair is
;; called a course spec vertex. The the line
;; containing both bouys bisects the angle formed by the
;; vertex's center point and the center points of the
;; preceeding and following course vertices. The sequence of
;; course spec vertices should be understood to loop, with the
;; last vertex preceding the first vertex.

;; The the course is the union of the quads formed from
;; the bouy locations for each pair of adjacent course vertices.

(s/defschema CourseSpec
  [{:center Vec, :width s/Num}])

(s/defschema Course
  [{:left Vec, :right Vec}])

(defn side
  "Returns :left if the point p is to the left of the vector from a to b,
  :colinear if it's colinear with a and b, and :right if it's to the right of the vector from a to b."
  [[ax ay] [bx by] [px py]]
  (let [det (- (* (- bx ax) (- py ay)) (* (- by ay) (- px ax)))]
    (cond
      (< det 0) :left
      (> det 0) :right
      :else     :colinear)))

(defn vbisect
  "Given points a,b,c, return the unit vector bisecting ba and bc.
  If the points are colinear, return the unit vector along ba
  rotated pi/2 radians clockwise."
  [a b c]
  (let [u (vnorm (vsub a b))
        v (vnorm (vsub c b))
        w (vadd u v)]
    (if (= [0.0 0.0] w)
      (vrot u (/ Math/PI 2))
      (vnorm (vadd u v)))))

(defn looped [col]
  (lazy-seq
   (concat col (looped col))))

(s/defn build-course :- Course
  [spec :- CourseSpec]
  (when (< (count spec) 3)
    (throw (ex-info "course spec must have at least three vertices" {:spec spec})))
  (let [vertices (looped (map :center spec))
        triples (map vector
                     (drop (dec (count spec)) vertices)
                     spec
                     (drop (inc (count spec)) vertices))]
    (for [[a {:keys [center width]} c] triples]
      (let [w (cond-> (vmul (vbisect a center c) [(/ width 2.0) (/ width 2.0)])
                (= :left (side a c center)) (vmul [-1 -1]))]
        {:left (vadd center w), :right (vsub center w)}))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Game tick

(s/defn handle-message :- GameState
  [game-state :- GameState
   msg :- [(s/one s/Str "client-id") (s/one IncomingMsg "msg")]]
  (let [[client-id [tag :as msg]] msg]
    (cond
      (= "connect" tag)
      (let [[game-state boat-id] (add-entity game-state (boat [0 0]))]
        (-> game-state
            (add-mediary client-id (PlayerClientMediary. boat-id))
            (add-outgoing client-id ["set-boat-id" boat-id])
            (add-outgoing client-id ["env-update"
                                     {:wind (:wind game-state)
                                      :current (:current game-state)}])))

      (= "disconnect" tag)
      (remove-mediary game-state client-id)

      :else
      (if-let [mediary (get-mediary game-state client-id)]
        (handle-message- mediary game-state msg)
        (do (println "Unprocessed message for" client-id ":" msg)
            game-state)))))

(s/defn tick :- GameState
  "Compute one game tick, producing a new game state"
  [game-state :- GameState]
  (as-> game-state %
    (reduce handle-message % (peek-all-incoming %))
    (pop-all-incoming %)
    (reduce-kv tick-entity % (:entities %))
    (broadcast-outgoing % ["tick" (:t %)])
    (update-in % [:t] inc)))

