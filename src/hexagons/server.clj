(ns hexagons.server
  (:require [cognitect.transit :as transit]
            [org.httpkit.server :as ws]
            [hexagons.state :as state])
  (:import [java.io ByteArrayInputStream ByteArrayOutputStream]
           [java.util.concurrent BlockingQueue LinkedBlockingQueue]))

(defn to-transit [value]
  (let [out (ByteArrayOutputStream. 4096)
        writer (transit/writer out :json)]
    (transit/write writer value)
    (.toString out)))

(defn from-transit [^String transit]
  (let [in (ByteArrayInputStream. (.getBytes transit))
        reader (transit/reader in :json {:handlers {"var" (transit/read-handler identity)}})]
    (transit/read reader)))

(defn send! [client message]
  (ws/send! client (to-transit message) false))

(defn ws-handler [req]
  (ws/with-channel req conn
    (ws/on-receive conn (fn [msg]
                          (let [event (from-transit msg)]
                            (state/handle-event conn event send!))))
    (ws/on-close conn (fn [status]
                        (state/handle-event conn [:disconnect])))))


(defn start! []
  (ws/run-server ws-handler {:port 9832 :join? false}))

(defn stop! [])

(do
  (stop!)
  (def stop!
    (start!)))
