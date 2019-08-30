(ns hexagons.math)

(def Tau (* 2 js/Math.PI))
(def sin js/Math.sin)
(def cos js/Math.cos)
(def R (sin (/ Tau 6)))

(defn m*v [m v]
  (mapv (fn [row]
          (apply + (map * row v))) m))

(defn transpose [m]
  (apply mapv vector m))

(defn m*m
  ([m1 m2]
   (let [m2 (transpose m2)]
     (mapv
      (fn [row]
        (mapv
         (fn [col]
           (apply + (map * row col)))
         m2))
      m1)))
  ([m1 m2 & mmore]
   (reduce m*m (m*m m1 m2) mmore)))

(defn scale-matrix [f]
  [[f 0 0]
   [0 f 0]
   [0 0 1]])

(defn translate-matrix [x y]
  [[1 0 0]
   [0 1 0]
   [x y 1]])
