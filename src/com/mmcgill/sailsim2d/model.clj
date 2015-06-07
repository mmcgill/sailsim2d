(ns com.mmcgill.sailsim2d.model
  "The sailing model and physics calculations."
  )

;;;;;; Constants ;;;;;;;;;;;;;;;;;;;;;;;;;

(def ticks-per-sec 30)
(def secs-per-tick (/ 1.0 ticks-per-sec))
(def water-rho 1000.0) ; kg/m^3
(def air-rho 1.225)    ; kg/m^3
(def wake-curve-ttl 5.0) ; seconds

;;;;;; Game state and ticks ;;;;;;;;;;;;;;

(defn game-state
  "Create a new game state."
  [wind-vec current-vec]
  {:t       0              ; tick number
   :wind    wind-vec       ; [x y]  meters/sec
   :current current-vec    ; [x y]  meters/sec
   :objects {}             ; map of id -> object
   :next-id 0})

(defn add-object
  ([game-state obj]
   (let [id (:next-id game-state)]
     (add-object (update-in game-state [:next-id] inc) obj id)))
  ([game-state obj id]
   [(update-in game-state [:objects] assoc id obj)
    id]))

(defn remove-object [game-state id]
  (update-in game-state [:objects] dissoc id))

(defn tick-object-dispatch [game-state id obj] (:type obj))
(defmulti tick-object #'tick-object-dispatch)

(defn tick
  "Compute one game tick, producing a new game state"
  [game-state]
  (-> (reduce-kv tick-object game-state (:objects game-state))
      (update-in [:t] inc)))

;;;;;; Boats ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn add-boat
  "Returns [game-state,id] where game-state is updated
  with a new boat identified by id."
  [game-state id pos]
  (add-object game-state
              {:type         :boat
               :sail-theta   0.0         ; angle, radians
               :rudder-theta 0.0         ; angle, radians
               :pos          pos         ; vector
               :v            [0.0 0.0]   ; vector
               :a            [0.0 0.0]   ; vector
               :theta        0.0         ; angle, radians
               :alpha        0.0         ; radians/sec^2
               :omega        0.0         ; raians/sec
               :mass         56.0        ; kg
               :length       3.6         ; m
               }
              id))

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

(defmethod tick-object :boat
  [{:keys [current] :as game-state}
   id
   {:keys [mass length pos v theta omega rudder-theta] :as boat}]
  ;; TODO: compute net force and torque, then apply to
  ;; velocity and position
  (let [current (vsub current v)
        tr (rudder-torque (/ length 2) current (+ theta rudder-theta))
        td (* (- rotation-damping) omega (/ length 2))
        t (+ tr td)
        fh (compute-drag water-rho current theta hull-cda)
        ;_ (prn :fh fh)
        fm (vrot [(* 2 mass) 0] theta)        ; TEMP: motor
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
    (assoc-in game-state [:objects id]
              (assoc boat
                     :pos pos :v v :a a
                     :theta theta :omega omega :alpha alpha))))

;;;;;; Wake Curves ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn add-wake-curve
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

(defn extend-wake-curve
  "Extend a wake curve with a new segment."
  [game-state id pos v]
  (update-in game-state [:objects id] conj
             {:pos pos :v v :ttl wake-curve-ttl}))

(defn update-wake-curve-point [{:keys [pos v]}])

(defmethod tick-object :wake-curve
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
     (first (add-boat state id [0 0])))

   "disconnect"
   (fn [state id _]
     (remove-object state id))

   "set-rudder-theta"
   (fn [state id theta]
     (if (get-in state [:objects id])
       (let [theta (cond
                     (> theta (/ Math/PI 4)) (/ Math/PI 4)
                     (< theta (/ Math/PI -4)) (/ Math/PI -4)
                     :else theta)]
         (assoc-in state [:objects id :rudder-theta] theta))
       state))})

(defn process-message
  [state [id [tag body]]]
  (if-let [handler (get message-handlers tag)]
    (handler state id body)
    (do (prn :unrecognized-message tag)
        state)))
