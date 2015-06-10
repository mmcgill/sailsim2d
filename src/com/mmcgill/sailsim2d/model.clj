(ns com.mmcgill.sailsim2d.model
  "The sailing model and physics calculations."
  (:require [schema.core :as s]))

;;;;;; Constants ;;;;;;;;;;;;;;;;;;;;;;;;;

(def ticks-per-sec 30)
(def secs-per-tick (/ 1.0 ticks-per-sec))
(def water-rho 1000.0) ; kg/m^3
(def air-rho 1.225)    ; kg/m^3
(def wake-curve-ttl 5.0) ; seconds
(def motor-accel 2.0) ; m/s^2

;;;;;; Game state and ticks ;;;;;;;;;;;;;;

(defprotocol Entity
  (update- [entity id game-state]))

(s/defschema Vec [(s/one s/Num "x") (s/one s/Num "y")])

(s/defschema Id s/Int)

(s/defschema GameState
  {:t       s/Int
   :wind    Vec
   :current Vec
   :entities {Id (s/protocol Entity)}
   :next-id Id})

(s/defn tick-entity :- GameState
  [game-state :- GameState, id :- Id, entity :- (s/protocol Entity)]
  (update- entity id game-state))

(s/defn game-state :- GameState
  "Create a new game state."
  [wind :- Vec, current :- Vec]
  {:t       0              ; tick number
   :wind    wind           ; [x y]  meters/sec
   :current current        ; [x y]  meters/sec
   :entities {}            ; map of id -> object
   :next-id 0})

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

(s/defn update-entity :- GameState
  [game-state :- GameState, id :- Id, entity :- (s/protocol Entity)]
  (update-in game-state [:entities] assoc id entity))

(s/defn remove-entity :- GameState
  [game-state :- GameState, id :- Id]
  (update-in game-state [:entities] dissoc id))

(s/defn tick :- GameState
  "Compute one game tick, producing a new game state"
  [game-state :- GameState]
  (-> (reduce-kv tick-entity game-state (:entities game-state))
      (update-in [:t] inc)))

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

(defrecord Boat [sail-theta rudder-theta pos v a theta
                 alpha omega mass length throttle]
  Entity
  (update- [_ id game-state]
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
          pos (vadd pos (vmul v [secs-per-tick secs-per-tick]))]
      (update-entity game-state id
                     (Boat. sail-theta rudder-theta pos v a theta
                            alpha omega mass length throttle)))))

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
           :throttle     s/Num}))

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
   ))

;;;;;; Wake Curves ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#_(defn add-wake-curve
  "Add a new wake curve to the game state, returning its id.

  A wake curve is a vector of points, each point having a position
  and velocity. The client renders the curve by drawing line segments
  between adjacent points. Each tick, each point's position is updated
  according to its velocity."
  [game-state pos v]
  (add-object
   game-state
   {:type   :wake-curve
    :points [{:pos pos, :v v, :ttl wake-curve-ttl}]}))

#_(defn extend-wake-curve
  "Extend a wake curve with a new segment."
  [game-state id pos v]
  (update-in game-state [:objects id] conj
             {:pos pos :v v :ttl wake-curve-ttl}))

#_(defn update-wake-curve-point [{:keys [pos v]}])

#_(defmethod tick-object :wake-curve
  [{:keys [current] :as game-state}
   id
   {:keys [id points]}]
  ;; update each point in the curve
  (if (empty? points)
    (remove-object game-state id))
  (->> points
       (filter #(> 0 (:ttl %)))
       (reduce update-wake-curve-point)
       (assoc-in game-state [:objects id :points])))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Message processing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def message-handlers
  {"connect"
   (fn [state id _]
     (first (add-entity state (boat [0 0]) id)))

   "disconnect"
   (fn [state id _]
     (remove-entity state id))

   "set-rudder-theta"
   (fn [state id theta]
     (if (get-in state [:entities id])
       (let [theta (cond
                     (> theta (/ Math/PI 4)) (/ Math/PI 4)
                     (< theta (/ Math/PI -4)) (/ Math/PI -4)
                     :else theta)]
         (assoc-in state [:entities id :rudder-theta] theta))
       state))

   "set-throttle"
   (fn [state id throttle]
     (if (get-in state [:entities id])
       (let [throttle (max 0 (min 1 throttle))]
         (assoc-in state [:entities id :throttle] throttle))
       state))})

(defn process-message
  [state [id [tag body]]]
  (if-let [handler (get message-handlers tag)]
    (handler state id body)
    (do (prn :unrecognized-message tag)
        state)))
