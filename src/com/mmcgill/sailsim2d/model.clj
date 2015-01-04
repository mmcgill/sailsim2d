(ns com.mmcgill.sailsim2d.model
  "The sailing model and physics calculations."
  )

(def ticks-per-sec 30)
(def secs-per-tick (/ 1.0 ticks-per-sec))
(def water-rho 1000.0) ; kg/m^3
(def air-rho 1.225)    ; kg/m^3

(defn boat
  "Return a new boat at the given position with default property values."
  [pos]
  {:sail-theta   0.0         ; angle, radians
   :rudder-theta 0.0         ; angle, radians
   :pos          pos         ; vector
   :v            [0.0 0.0]   ; vector
   :a            [0.0 0.0]   ; vector
   :theta        0.0         ; angle, radians
   :alpha        0.0         ; radians/sec^2
   :omega        0.0         ; raians/sec
   :mass         56.0        ; kg
   :length       3.6         ; m
   })

(defn game-state
  "Create a new game state."
  [wind-vec current-vec]
  {:t       0              ; tick number
   :wind    wind-vec       ; [x y]  meters/sec
   :current current-vec    ; [x y]  meters/sec
   :boats   {}             ; map of id -> boat
   })

(defn add-boat [game-state id boat]
  (update-in game-state [:boats] assoc id boat))

(defn remove-boat [game-state id]
  (update-in game-state [:boats] dissoc id))

(defn compute-drag
  "Compute force of drag as a vector: (* (1/2) rho v v (cd theta)),
   where theta is the angle of v.

   rho:   mass density of fluid
   v:     relative velocity of fluid as a vector
   theta: orientation of the solid body
   cda:   [theta -> drag coefficient * area]"
  [rho v theta cda]
  (let [[x y] v
        fluid-theta (- (Math/atan2 y x) theta)
        k (* 0.5 rho (cda fluid-theta))]
    [(* k x x) (* k y y)]))

(defn hull-cda [theta]
  ;; TODO: Use interpolation and pre-calculated table for hull shape
  (let [k (Math/cos (+ (* 2 theta) Math/PI))]
    (+ 1.5 (* k (+ 2.2 (* 0.8 k))))))

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

(def rudder-coefficient "Scales the rudder torque"      0.05)
(def rotation-damping   "Damps rotation of the boat"    1.5)

(defn rudder-torque [r current rudder-theta]
  (let [theta (- rudder-theta (vangle current))
        speed (vmag current)]
    (* r rudder-coefficient speed speed (+ (Math/sin theta) #_(Math/sin (* 3 theta))))))

(defn tick-boat [{:keys [current] :as game-state}
                 {:keys [mass length pos v theta omega rudder-theta] :as boat}]
  ;; TODO: compute net force and torque, then apply to
  ;; velocity and position
  (let [current (vsub current v)
        tr (rudder-torque (/ length 2) current (+ theta rudder-theta))
        td (* (- rotation-damping) omega (/ length 2))
        t (+ tr td)
        fh (compute-drag water-rho current theta hull-cda)
        ;_ (prn :fh fh)
        f fh

        ;; rotational dynamics
        alpha (/ t (moment-of-inertia length mass))
        omega (+ omega (* secs-per-tick alpha))
        theta (+ theta (* secs-per-tick omega))

        ;; linear dynamics
        ;;a (vdiv f [mass mass])
        ;;v (vadd v (vmul a [secs-per-tick secs-per-tick]))
        pos (vadd pos (vmul v [secs-per-tick secs-per-tick]))]
    (assoc boat
           ;;:pos pos :v v :a a
           :theta theta :omega omega :alpha alpha)))

(defn tick-boats [game-state boats]
  (reduce-kv #(assoc %1 %2 (tick-boat game-state %3)) {} boats))

(defn tick
  "Compute one game tick, producing a new game state"
  [game-state]
  (-> game-state
      (update-in [:boats] #(tick-boats game-state %))
      (update-in [:t] inc)))
