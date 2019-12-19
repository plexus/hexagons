(ns hexagons.macros)

(defmacro for*
  {:style/indent 1}
  [& args]
  `(doall (for ~@args)))

(defmacro defreaction
  {:style/indent 1}
  [name & body]
  `(def ~name (reagent.ratom/make-reaction (fn [] ~@body))))
