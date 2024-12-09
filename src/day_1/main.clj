(ns day_1.main
  (:require [clojure.string :as str]))

(defn split [x] (mapv #(Integer/parseInt %) (str/split x #"\s+")))

(def data
  (reduce
    (fn [acc item]
      {:left  (conj (:left acc) (first (split item)))
       :right (conj (:right acc) (last (split item)))})
    {:left [] :right []}
    (-> "src/day_1/input"
        slurp
        str/split-lines
        vec)
    )
  )

(def difference
  (reduce
    (fn [acc [l r]] (+ acc (Math/abs (- l r))))
    0
    (map vector (sort (:left data)) (sort (:right data)))
    )
  )

(println difference)

(def similarity
  (reduce
    (fn [acc l]
      (+ acc (* l (get (frequencies (:right data)) l 0)))
      )
    0
    (:left data)
    )
  )

(println similarity)
