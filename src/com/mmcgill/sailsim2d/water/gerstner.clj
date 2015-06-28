(ns com.mmcgill.sailsim2d.water.gerstner
  (:require [mikera.image.core :as img]
            [mikera.image.colours :as c]
            [schema.core :as s]
            [com.mmcgill.sailsim2d.model :as m])
  (:import [java.awt.image BufferedImage]))

(def G 9.8)

(s/defschema GerstnerWaveParams
  {:amplitude s/Num
   :wavelength s/Num
   :direction m/Vec})

(s/defschema InternalWaveParams
  {:amplitude s/Num
   :wave-vector m/Vec
   :speed s/Num
   :frequency s/Num})

(s/defn precalc-params :- InternalWaveParams
  [params :- GerstnerWaveParams]
  (let [{:keys [amplitude wavelength direction]} params
        speed (/ (* 2 Math/PI) wavelength)
        kx (* speed (first direction))
        ky (* speed (second direction))
        freq (Math/sqrt (* G (/ (* 2 Math/PI) wavelength)))]
    {:amplitude amplitude
     :wave-vector [kx ky]
     :speed speed
     :frequency freq}))

(s/defn gerstner-point :- [(s/one m/Vec "point"), (s/one s/Num "height")]
  [wave-params :- [InternalWaveParams]
   t :- s/Num
   p0 :- m/Vec]
  "Returns [[x y] h], where h is the height of the wave at
  position [x y], which is the displayed position of the
  input [x0 y0]."
  (let [[x0 y0] p0]
    (loop [[{:keys [amplitude wave-vector speed frequency]} & xs] wave-params
           x 0
           y 0
           h 0]
      (let [[kx ky] wave-vector
            theta (- (+ (* x0 kx) (* y0 ky)) (* frequency t))
            disp (* amplitude (Math/sin theta))
            x (+ x (* (/ kx speed) disp))
            y (+ y (* (/ ky speed) disp))
            h (+ h (* amplitude (Math/cos theta)))]
        (if (seq xs)
          (recur xs x y h)
          [[(- x0 x) (- y0 y)] h])))))

(s/defn gerstner-profile :- BufferedImage
  [wave-params :- [GerstnerWaveParams]
   t :- s/Num
   width :- s/Int
   height :- s/Int
   pixels-per-meter :- s/Num]
  (let [image (img/new-image width height)
        pixels (img/get-pixels image)
        params (map precalc-params wave-params)]
    (doseq [px (range width)]
      (let [x0 (/ px pixels-per-meter)
            [[x y] h] (gerstner-point params t [x0 0])
            px (int (* x pixels-per-meter))
            py (int (- (/ height 2) (* h pixels-per-meter)))]
        (when (and (<= 0 px (dec width))
                   (<= 0 py (dec height)))
          (aset pixels (+ (* py width) px)
                (unchecked-int (c/rgb 1 0 0))))))
    (img/set-pixels image pixels)
    image))

(defn animated-gerstner-profile
  [wave-params
   seconds
   fps
   width height
   pixels-per-meter]
  (let [millis-per-frame (* 1000 (/ 1.0 fps))]
    (dotimes [f (int (* seconds fps))]
      (let [t (* f (/ 1.0 fps))
            millis (System/currentTimeMillis)
            image (gerstner-profile wave-params t width height pixels-per-meter)]
        (img/show image)
        (Thread/sleep (max 0 (- millis-per-frame
                                (- (System/currentTimeMillis) millis))))))))

(defn gerstner-water
  [width height]
  (let [image (img/new-image width height)
        pixels (img/get-pixels image)]
    (doseq [x (range width)
            y (range height)]
      (aset pixels (+ (* y width) x)
            (unchecked-int (c/rgb (/ x (float width))
                                  (/ y (float height))
                                  0.5))))
    (img/set-pixels image pixels)
    image))
