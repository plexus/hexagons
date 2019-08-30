(ns hexagons.tintin
  (:require [clojure.java.io :as io]
            [hexagons.lab-diff :as diff]
            [clj-ml.data :as mld]
            [clj-ml.clusterers :as mlc]
            [net.cgrand.xforms :as x])
  (:import [java.awt.image BufferedImage]
           [javax.imageio ImageIO]
           [hexagons ColorConverter]
           [weka.core
            DistanceFunction
            EuclideanDistance
            Instance]))

(set! clojure.core/*unchecked-math* :warn-on-boxed)

(defn rgb->lab [[r g b]]
  (ColorConverter/RGBtoLAB r g b ColorConverter/CIE10_D65))

(defn lab->rgb [[l a b]]
  (into [] (ColorConverter/LABtoRGB l a b ColorConverter/CIE10_D65)))

(defn rgb->hls [[r g b]]
  (into [] (ColorConverter/RGBtoHLS r g b)))

(defn inst->vec [^Instance s]
  (vec (.toDoubleArray s)))

(defn get-rgb [img cord]
  (let [[x y] cord
        ^long clr (.getRGB img x y)]
    [(bit-and (bit-shift-right clr 16) 0xff)
     (bit-and (bit-shift-right clr 8) 0xff)
     (bit-and clr 0xff)]))

(comment

  (def img-files
    (dissoc (group-by #(re-find #"\d+" (str %)) (sort (file-seq (io/file (io/resource "tintin")))))
            nil))

  (defn pixels [file]
    (into []
          (comp
           (map (partial bit-and 0xFF))
           (x/partition 3)
           (map reverse)
           (map rgb->lab))
          (->> file
               ImageIO/read
               .getRaster
               .getDataBuffer
               .getData)))

  (defn rand-n [coll count]
    (repeatedly count #(rand-nth coll)))

  (defn find-colors [pixels color-count]
    (let [ds (mld/make-dataset "pixels" [:l :a :b] pixels)
          clusterer (mlc/make-clusterer :k-means {:number-clusters color-count})]
      (mlc/clusterer-build clusterer ds)
      (mapv (comp lab->rgb inst->vec)
            (.getClusterCentroids clusterer))))

  (doseq [[num files] (sort img-files)
          [file idx] (map vector files (range))]
    (time
     (do
       (print num idx "...")
       (let [colors (-> file
                        pixels
                        (rand-n 25000)
                        (find-colors 20)
                        #_(->> (sort-by rgb->hls))
                        sort
                        reverse)]
         (spit (format "resources/public/colors-%s-%02d.edn" num idx)
               (with-out-str
                 (clojure.pprint/pprint colors)))
         (println "ok")))))


  (def all-colors
    (->> "resources/public"
         io/file
         file-seq
         (filter #(re-find #"\.edn$" (str %)))
         (mapcat (comp read-string slurp))
         (map rgb->lab)))



  )
