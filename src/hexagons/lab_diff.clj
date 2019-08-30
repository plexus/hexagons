(ns hexagons.lab-diff)

(set! clojure.core/*unchecked-math* :warn-on-boxed)

;; port of https://github.com/markusn/color-diff/blob/master/lib/diff.js

;; weight factors
(def kL 1)
(def kC 1)
(def kH 1)

(defn sqrt ^double [^double x]
  (Math/sqrt x))

(defn pow ^double [^double x ^double y]
  (Math/pow x y))

(defn deg->rad ^double [^double deg]
  (/ (* deg (double Math/PI)) 180))

(defn rad->deg ^double [^double rad]
  (/ (* rad 180) (double Math/PI)))

(defn hp-f [x y]
  (if (and (= 0 x) (= 0 y))
    0
    (let [hp (rad->deg (Math/atan2 x y))]
      (if (>= hp 0)
        hp
        (+ 360 hp)))))

(defn dhp-f [^double c1 ^double c2 ^double h1p ^double h2p]
  (if (= 0 (* c1 c2))
    0
    (let [delta (- h2p h1p)]
      (cond
        (<= (Math/abs delta) 180)
        delta

        (> delta 180)
        (- delta 360)

        (< delta -180)
        (+ delta 360)

        :else
        (assert "bad delta" delta)))))

(defn a-hp-f ^double [^double c1 ^double c2 ^double h1p ^double h2p]
  (if (= 0 (* c1 c2))
    (+ h1p h2p)
    (let [delta (Math/abs (- h1p h2p))
          sum (+ h1p h2p)]
      (cond
        (<= delta 180)
        (/ sum 2)

        (and (> delta 180) (< sum 360))
        (/ (+ sum 360) 2)

        (and (> delta 180) (>= sum 360))
        (/ (- sum 360) 2)

        :else
        (assert "bad delta" delta)))))

(defn ciede2000 ^double [[^double l1 ^double a1 ^double b1] [^double l2 ^double a2 ^double b2]]
  (try
    (let [c1 (sqrt (+ (pow a1 2)
                      (pow b1 2)))
          c2 (sqrt (+ (pow a2 2)
                      (pow b2 2)))

          a-c1-c2 (/ (+ c1 c2) 2)

          g (* 0.5 (- 1 (sqrt (/ (pow a-c1-c2 7)
                                 (+ (pow a-c1-c2 7)
                                    (pow 25 7))))))

          a1p (* (+ g 1) a1)
          a2p (* (+ g 1) a2)

          c1p (sqrt (+ (pow a1p 2) (pow b1 2)))
          c2p (sqrt (+ (pow a2p 2) (pow b2 2)))

          h1p (hp-f b1 a1p)
          h2p (hp-f b2 a2p)

          dLp (- l2 l1)
          dCp (- c2p c1p)

          dhp (dhp-f c1 c2 h1p h2p)
          dHp (* 2 (sqrt (* c1p c2p)) (Math/sin (/ (deg->rad dhp) 2)))

          a-l  (/ (+ l1 l2) 2)
          a-Cp (/ (+ c1p c2p) 2)

          a-hp (a-hp-f c1 c2 h1p h2p)

          T (- (+ (- 1 (* 0.17 (Math/cos (deg->rad (- a-hp 30)))))
                  (* 0.24 (Math/cos (deg->rad (* 2 a-hp))))
                  (* 0.32 (Math/cos (deg->rad (+ (* 3 a-hp) 6)))))
               (* 0.20 (Math/cos (deg->rad (- (* 4 a-hp) 63)))))

          d-ro (* 30 (Math/exp (- (pow (/ (- a-hp 275) 25) 2))))
          rc   (sqrt (/ (pow a-Cp 7)
                        (+ (pow a-Cp 7)
                           (pow 25 7))))
          sl (+ 1 (/ (* 0.0150 (pow (- a-l 50) 2))
                     (sqrt (+ 20 (pow (- a-l 50) 2)))))

          sc (+ 1 (* 0.045 a-Cp))
          sh (+ 1 (* 0.015 a-Cp T))
          rt (* -2 rc (Math/sin (deg->rad (* 2 d-ro))))]
      (sqrt (+ (pow (/ dLp (* sl (double kL))) 2)
               (pow (/ dCp (* sc (double kC))) 2)
               (pow (/ dHp (* sh (double kH))) 2)
               (* rt
                  (/ dCp (* sc (double kC)))
                  (/ dHp (* sh (double kH)))))))
    (catch Exception e
      (throw (ex-info "Problem" {:l1 [l1 a1 b1]
                                 :l2 [l2 a2 b2]})))))
