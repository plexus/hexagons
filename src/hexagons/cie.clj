(ns hexagons.cie)


;; Thanks Christopher!
;; http://christopherbird.co.uk/posts/finding-similar-colors/
(defn ciexyz-adjust [component]
  (* 100
     (let [x (/ component 255)]
       (if (<= x 0.04045)
         (/ x 12.92)
         (Math/pow (/ (+ x 0.055) 1.055) 2.4)))))

(defn ciexyz-readjust [component]
  (* 255
     (let [x (/ component 100)]
       (if (<= x 0.0031308)
         (* x 12.92)
         (- (* (Math/pow x (/ 1 2.4)) 1.055) 0.55)))))

(defn rgb->ciexyz [[r g b]]
  (let [r (ciexyz-adjust r)
        g (ciexyz-adjust g)
        b (ciexyz-adjust b)]
    [(float (+ (* 0.4124 r) (* 0.3576 g) (* 0.1805 b)))
     (float (+ (* 0.2126 r) (* 0.7152 g) (* 0.0722 b)))
     (float (+ (* 0.0193 r) (* 0.1192 g) (* 0.9505 b)))]))

(defn ciexyz->rgb [[x y z]]
  [(ciexyz-readjust (float (+ (* 3.2406 x) (* -1.5372 y) (* -0.4986 z))))
   (ciexyz-readjust (float (+ (* -0.9689 x) (* 1.8758 y) (* 0.0415 z))))
   (ciexyz-readjust (float (+ (* 0.0557 x) (* -0.2040 y) (* 1.0570 z))))])


(ciexyz->rgb
 (rgb->ciexyz [255 161 164]))



(defn cielab-adjust [component]
  (if (> component 0.008856)
    (Math/pow component (/ 1 3))
    (+ (* 7.787 component) (16 / 116))))

(defn ciexyz->cielab [[x y z]]
  (let [x (cielab-adjust (/ x 95.047))
        y (cielab-adjust (/ y 100.000))
        z (cielab-adjust (/ z 108.883))]
    [(float (if (> z 0.008856) (- (* 116 y) 16) (* 903.3 y)))
     (float (* 500 (- x y)))
     (float (* 200 (- y z)))]))
