(ns hexagons.game)

(def colors
  {:black [44 44 40]
   :white [241 242 238]
   :gray1 [200 205 202]
   :gray2 [163 160 150]
   :gray3 [118 120 115]
   :warm-gray [82 81 69]
   :blue1 [134 190 215]
   :blue2 [89 145 193]
   :blue3 [60 98 165]
   :green1 [150 178 107]
   :green2 [96 130 84]
   :sand [230 216 166]
   :yellow [227 207 85]
   :tan [213 173 134]
   :red [211 72 44]
   :orange1 [215 124 75]
   :orange2 [214 159 77]
   :brown1 [149 112 75]
   :brown2 [121 62 40]
   :brown3 [61 29 44]})

(defn rand-shape []
  (inc (rand-int 5)))

(defn rand-tile []
  {:color  (rand-nth (keys colors))
   :tokens (inc (rand-int 5))
   :shape  (rand-shape)})

(defn rand-action []
  [(rand-nth [:get :steal :give])
   (inc (rand-int 3))
   (rand-nth [:card :token])])

(defn rand-card []
  {:shapes (doall (repeatedly (+ (rand-int 3) 3) rand-shape))
   :actions (doall (repeatedly (inc (rand-int 2)) rand-action))})

(rand-card)
