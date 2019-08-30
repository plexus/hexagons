(ns ^:figwheel-hooks hexagons.ui
  (:require [goog.dom :as gdom]
            [goog.events :as events]
            [reagent.core :as reagent]
            [clojure.string :as str]
            [goog.string.format]
            [cljs.tools.reader.edn :as edn]
            [cognitect.transit :as transit]
            [hexagons.game :as game]
            [hexagons.math :refer [Tau sin cos m*m m*v transpose scale-matrix translate-matrix]]
            [hexagons.websocket :as ws])
  (:import (goog.events EventTarget EventType)))

(when (= "" js/document.location.hash)
  (set! js/document.location.hash (random-uuid)))

(def game-id (subs js/document.location.hash 1))

(let [cid (or (.getItem js/localStorage (str game-id "-client-id"))
              (str (random-uuid)))]
  (.setItem js/localStorage (str game-id "-client-id") cid)
  (def client-id cid))

(declare socket)

(def transit-writer (transit/writer :json {}))

(defn to-transit [value]
  (transit/write transit-writer value))

(defn from-transit [string]
  (transit/read (transit/reader :json) string))

(defn send! [message]
  (assert (ws/open? socket))
  (when (ws/open? socket)
    (ws/send! socket (to-transit (into [game-id client-id] message)))))

(defmulti handle-event (fn [e & _] e))

(def socket
  (ws/connect! "ws://localhost:9832"
               {:open
                (fn [e]
                  (send! [:connect]))

                :message
                (fn [e]
                  (apply handle-event (from-transit (ws/message-data e))))}))

(defn query-viewport-size []
  {:width js/document.documentElement.clientWidth
   :height js/document.documentElement.clientHeight})

(defonce state (reagent/atom (query-viewport-size)))

(defmethod handle-event :game-state [_ new-state]
  (swap! state merge new-state))

(set! js/document.body.onresize (fn [_] (swap! state merge (query-viewport-size))))

(defn color-rgb [color]
  (let [[r g b] (get game/colors color)]
    (str "rgb(" r "," g "," b ")")))

(defn polygon-corner
  [n i]
  (let [rad (+ (* (/ Tau n) i) (/ Tau 2))]
    [(sin rad) (cos rad) 1]))

(defn star-point
  [n i inner outer]
  (let [rad (+ (* (/ Tau n) i) (/ Tau 2))
        length (if (odd? i) inner outer)]
    [(* (sin rad) length) (* (cos rad) length) 1]))

(defn point-str [m]
  (str/join " " (map (fn [[x y]] (str x "," y)) m)))

(defn hex-coord [x y]
  (let [R (sin (/ Tau 6))]
    (translate-matrix
     (+ (* y R) (* 2 x R))
     (* 1.5 y))))

(defn hex-neighbors [[x y]]
  #{[(inc x) y 1]
    [(dec x) y 1]
    [(inc x) (dec y) 1]
    [(dec x) (inc y) 1]
    [x (inc y) 1]
    [x (dec y) 1]})

(defn neighbor-set [tiles]
  (let [coords (into #{} (map :coords) tiles)]
    (into #{}
          (comp (mapcat hex-neighbors)
                (remove coords))
          coords)))

#_
(swap! state assoc :next-tile (game/rand-tile))

(defn indexed [coll]
  (map vector (range) coll))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; State queries

(defn my-turn? []
  (= client-id (:active-player @state)))

(defn players []
  (map (fn [{:keys [id] :as client}]
         (cond-> client
           (= client-id id)
           (assoc :me? true)

           (= id (:active-player @state))
           (assoc :active? true)))
       (:clients @state)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Event handlers

(defn start-hover! [coords]
  (when (my-turn?)
    (swap! state assoc :hover coords)
    (send! [:start-hover! coords])))

(defn end-hover! []
  (when (my-turn?)
    (swap! state dissoc :hover)
    (send! [:end-hover!])))

(defn place-tile! []
  (when (my-turn?)
    (let [tile (:hover @state)]
      (end-hover!)
      (send! [:place-tile tile]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Components

(defn regular-polygon [x y sides size scale opts]
  [:polygon (merge {:points (point-str
                             (m*m
                              (map (comp (partial m*v (scale-matrix size))
                                         (partial polygon-corner sides))
                                   (range sides))
                              (hex-coord x y)
                              (scale-matrix scale)))
                    :stroke (color-rgb :black)
                    :fill (color-rgb :red)
                    :stroke-width 1.5}
                   opts)])

(defn star [x y sides size scale opts]
  [:polygon (merge {:points (point-str
                             (m*m
                              (map (comp (partial m*v (scale-matrix size))
                                         #(star-point sides % 0.3 1))
                                   (range sides))
                              (hex-coord x y)
                              (scale-matrix scale)))
                    :stroke (color-rgb :black)
                    :fill (color-rgb :purple)
                    :stroke-width 1.5}
                   opts)])

(defn hexagon [x y & [opts]]
  [regular-polygon x y 6 1 50 opts])

(defn tile [{:keys [coords color shape tokens fixed?]}]
  (let [[x y] coords]
    [:g (if fixed?
          {:on-mouse-over #(end-hover!)}
          {:opacity 0.5
           :on-click #(place-tile!)})
     [hexagon x y {:fill (color-rgb color)}]
     (case shape
       1
       (let [[[cx cy]] (m*m [[0 0 1]]
                            (hex-coord x y)
                            (scale-matrix 50))]
         [:circle {:cx cx
                   :cy cy
                   :r 22
                   :fill (color-rgb :red)
                   :stroke-width 1.5
                   :stroke (color-rgb :black)}])

       2
       [star x y 12 0.45 50 {:stroke-width 1.5
                             :fill (color-rgb :red)}]

       [regular-polygon x y shape 0.45 50 {:stroke-width 1.5
                                           :fill (color-rgb :red)}])
     (for [[x y] (m*m (map (comp
                            (partial m*v (scale-matrix 0.67))
                            (partial polygon-corner tokens))
                           (range tokens))
                      (hex-coord x y)
                      (scale-matrix 50))]
       ^{:key (str x "-" y)}
       [:circle {:cx x
                 :cy y
                 :r 5
                 :fill :white
                 :stroke :black}])
     ]))

(defn drop-zone [[x y :as coords]]
  [hexagon x y {:fill "rgba(0,0,0,0)"
                :stroke-width 0
                :on-mouse-over #(start-hover! coords)}])

(defn app-root []
  (let [{:keys [width height tiles hover next-tile]} @state
        top-left-x (/ width -2)
        top-left-y (/ height -2)]
    [:svg {:width width
           :height height
           :viewBox (str/join " " [top-left-x
                                   top-left-y
                                   width
                                   height])}
     [:rect {:x top-left-x
             :y top-left-y
             :width width
             :height height
             :fill "rgba(0,0,0,0)"
             :on-mouse-over #(end-hover!)}]

     (for [[i {:keys [name me? active?]}] (indexed (players))]
       [:g
        (when active?
          [:circle {:cx (+ top-left-x 20)
                    :cy (+ top-left-y 24 (* i 30))
                    :r 7
                    :fill (color-rgb :red)}])
        [:text {:x (+ top-left-x 40)
                :y (+ top-left-y 30 (* i 30))
                :font-family "sans-serif"}
         name (when me?
                " (me)")]]
       )

     (for [{[x y] :coords :as t} tiles]
       ^{:key (str x "-" y)}
       [tile (assoc t :fixed? true)])

     (for [[x y :as coords] (neighbor-set tiles)]
       ^{:key (str x "-" y)}
       [drop-zone coords])

     (when (and hover next-tile)
       [tile (assoc next-tile
               :coords hover
               :fixed? false)])
     ]))


(defn mount [el]
  (reagent/render-component [app-root] el))

(defn mount-app-element []
  (when-let [el (gdom/getElement "app")]
    (mount el)))

(mount-app-element)

(defn ^:after-load on-reload []
  (mount-app-element))
