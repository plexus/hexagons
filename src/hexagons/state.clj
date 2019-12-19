(ns hexagons.state
  (:require [hexagons.game :as game]))

(defonce !games (atom {}))
(defonce !clients (atom {}))

(defn new-game [uuid]
  {:id uuid
   :clients []
   :tiles #{{:coords [0 0 1]
             :color :green1
             :tokens 6
             :shape 3}}
   :next-tile (game/rand-tile)
   :phase :play-tile})

(defn new-client [uuid name]
  {:id     uuid
   :name   name
   :tokens 3
   :cards  (repeatedly 3 game/rand-card)})

(defn get-game [uuid]
  (or (get @!games uuid)
      (let [game (new-game uuid)]
        (swap! !games assoc uuid game)
        game)))

(defn get-client [gid cid]
  (let [{:keys [clients] :as game} (get-game gid)]
    (some #(when (= (:id %) cid) %) clients)))

(defn add-client [gid cid]
  (let [{clients :clients :as game} (get-game gid)
        client (new-client cid (str "Player " (inc (count clients))))]
    (swap! !games (fn [games]
                    (-> games
                        (update-in [gid :clients] conj client)
                        (update-in [gid :active-player] #(or % cid)))))
    client))

(def -handle-event nil)
(defmulti -handle-event (fn [gid cid e & _] e))

(defn handle-event [conn [gid cid type :as evt] send-fn]
  (try
    (.println java.lang.System/out (prn-str (into [] (drop 2) evt)))
    (case type
      :connect
      (do (or (get-client gid cid)
              (add-client gid cid))
          (swap! !clients assoc cid conn))

      :disconnect
      (do)

      #_:else
      (swap! !games update gid
             (fn [game]
               (apply -handle-event game (next evt)))))

    (let [game (get-game gid)
          clients @!clients]
      (doseq [{cid :id} (:clients game)]
        (send-fn (get clients cid) [:game-state game])))

    (catch Exception e
      (def ex e)
      (send-fn (get @!clients cid) [:exception (str e)]))))

(defn next-player [{:keys [clients active-player] :as game}]
  (->> clients
       cycle
       (partition 2 1)
       (some (fn [[c1 c2]]
               (when (= (:id c1) active-player)
                 (:id c2))))
       (assoc game :active-player)))

(defmethod -handle-event :place-tile [{:keys [next-tile] :as game} _ _ [x y :as coords]]
  (-> game
      (update :tiles conj (assoc next-tile :coords coords))
      (assoc :next-tile (game/rand-tile)
             :hover nil
             :phase :buy-or-cast)))

(defmethod -handle-event :start-hover! [game _ _ coords]
  (assoc game :hover coords))

(defmethod -handle-event :end-hover! [game _ _]
  (assoc game :hover nil))
