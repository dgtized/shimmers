(ns shimmers.math.control
  (:require
   [shimmers.math.equations :as eq]
   [thi.ng.math.core :as tm]))

(defn angular-delta [angle target]
  (let [delta (- (mod target eq/TAU) (mod angle eq/TAU))]
    (cond (< delta (- Math/PI)) (+ delta eq/TAU)
          (> delta Math/PI) (- delta eq/TAU)
          :else delta)))

;; see also https://gamedev.stackexchange.com/questions/1885/target-tracking-when-to-accelerate-and-decelerate-a-rotating-turret
(defn angular-acceleration [angle target control angle-vel]
  (let [delta (angular-delta angle target)]
    (- (* control delta)
       (* (* 2 (Math/sqrt control)) angle-vel))))

(defn spin-acceleration [angle-vel target-vel control]
  (let [delta (- target-vel angle-vel)]
    (- (* control delta)
       (* (* 2 (Math/sqrt control)) angle-vel))))

(defn force-accel [pos target control velocity]
  (let [dir (tm/- target pos)]
    (tm/- (tm/* dir control)
          (tm/* velocity (* 2 (Math/sqrt control))))))

;; see also http://brettbeauregard.com/blog/tag/beginners-pid/
(defprotocol IPid
  (adjust [_ time-ms value]))

(defrecord PID [set-point i-term kp ki kd last-time last-value bounds]
  IPid
  (adjust [pid time-ms value]
    (if (and last-time (>= (- time-ms last-time) 0.0))
      (let [[in0 in1 out0 out1] bounds
            dt (- time-ms last-time)
            current-value (tm/map-interval-clamped value [in0 in1] [-1.0 1.0])
            expected (tm/map-interval-clamped set-point [in0 in1] [-1.0 1.0])
            error (- expected current-value)
            derivative (if last-value
                         (let [last-val (tm/map-interval-clamped last-value [in0 in1] [-1.0 1.0])]
                           (/ (- error (- expected last-val)) dt))
                         0.0)
            integral (+ i-term (* error dt))
            control (tm/map-interval-clamped
                     (+ (* kp error) (* ki integral) (* kd derivative))
                     [-1.0 1.0]
                     [out0 out1])]
        (assoc pid
               :i-term integral
               :last-value value
               :last-time time-ms
               :control control))
      (assoc pid :last-time time-ms))))

(defn make-pid [pid]
  (map->PID
   (merge
    {:i-term 0.0
     :set-point 0.0
     :control 0.0
     :last-time nil
     :last-value nil
     :bounds [-1.0 1.0 -1.0 1.0]}
    pid)))

(comment
  (let [dt 0.02
        max-accel (* 1.0 dt)]
    (->> (range 0 100.0 dt)
         (reductions (fn [[pid _t pos vel _accel] t]
                       (let [pid' (adjust pid (* t 1000.0) pos)
                             accel (tm/clamp (or (:control pid') 0.0)
                                             (- max-accel) max-accel)
                             vel (+ vel accel)]
                         [pid'
                          t
                          (+ pos vel)
                          (* vel 0.99)
                          accel]))
                     [(make-pid {:kp 0.2 :ki 0.05 :kd (/ 0.2 4)
                                 :set-point 20.0
                                 :bounds [-100.0 100.0 -100.0 100.0]})
                      0.0
                      0.0
                      0.0
                      0.0])
         (map (fn [[_pid time pos vel accel]]
                [time pos vel accel])))))
