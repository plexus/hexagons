(ns ^:figwheel-hooks hexagons.colors
  (:require [goog.dom :as gdom]
            [goog.events :as events]
            [reagent.core :as reagent]
            [clojure.string :as str]
            [goog.string.format]
            [cljs.tools.reader.edn :as edn])
  (:import (goog.events EventTarget EventType)))

(def state
  (reagent/atom
   {:width js/window.innerWidth
    :height js/window.innerHeight}))

(def colors
  {:orange1   [215 124 75]
   :blue1     [134 190 215]
   :blue2     [89 145 193]
   :yellow    [227 207 85]
   :brown1    [149 112 75]
   :orange2   [214 159 77]
   :gray2     [163 160 150]
   :white     [241 242 238]
   :brown2    [121 62 40]
   :black     [44 44 40]
   :green2    [96 130 84]
   :blue3     [60 98 165]
   :brown3    [61 29 44]
   :gray1     [200 205 202]
   :tan       [213 173 134]
   :warm-gray [82 81 69]
   :green1    [150 178 107]
   :sand      [230 216 166]
   :red       [211 72 44]
   :gray3     [118 120 115]})

(def color-order [:black
                  :white
                  :gray1
                  :gray2
                  :gray3
                  :warm-gray
                  :blue1
                  :blue2
                  :blue3
                  :green1
                  :green2
                  :sand
                  :yellow
                  :tan
                  :red
                  :orange1
                  :orange2
                  :brown1
                  :brown2
                  :brown3])

(defn get-app-element []
  (gdom/getElement "app"))

(defn hello-world []
  (let [{:keys [width height tiles hover]} @state]
    [:svg {:width width
           :height height
           :viewBox (str/join " " [(/ width -2)
                                   (/ height -2)
                                   width
                                   height
                                   ])}

     (for [[i color] (map vector (range) color-order)
           :let [[r g b] (get colors color)]]
       [:g
        [:circle {:cx 0
                  :cy (- (* i 50) 505)
                  :r 19
                  :fill (str "rgb(" r "," g "," b ")")
                  :stroke "rgb(44,44,40)"
                  :stroke-width 2}]
        [:text {:y (- (* i 50) 500)
                :x 50}
         (name color)]
        [:text {:y (- (* i 50) 500)
                :x 150}
         (str "[" r ", " g ", " b "]")]

        [:text {:y (- (* i 50) 500)
                :x 300}

         (let [hex (fn [h] (let [x (.toString h 16)] (if (= 1 (count x)) (str "0" x) x)))]
           (str "#" (hex r) (hex g) (hex b)))]
        ])]))


(defn mount [el]
  (reagent/render-component [hello-world] el))

(defn mount-app-element []
  (when-let [el (get-app-element)]
    (mount el)))

(mount-app-element)

(defn ^:after-load on-reload []
  (mount-app-element))


(set! js/document.body.onresize
      (fn [_]
        (swap! state
               assoc
               :width js/window.innerWidth
               :height js/window.innerHeight)))
