(ns day_2.main
  (:require [clojure.string :as str]))

(defn split [x]
  (mapv #(Integer/parseInt %) (str/split x #"\s+")))

(def data
  (map split
       (-> "src/day_2/input"
           slurp
           str/split-lines
           vec)))

(defn monotone? [v]
  (or (apply < v)
      (apply > v)))

(defn speed [in]
  (rest
    (first
      (reduce
        (fn [[res prev] x] [(conj res (Math/abs (- prev x))) x])
        [[] 0]
        in)
      )
    )
  )

(defn safe-speed? [speed]
  (and (>= speed 1)
       (<= speed 3)))

(defn safe? [v]
  (and (every? true? (map safe-speed? (speed v)))
       (monotone? v)))

; solution for part 1
(count (filter true? (map safe? data)))

(defn remove-each [v]
  (mapv #(vec (concat (subvec v 0 %) (subvec v (inc %)))) (range (count v))))

; solution for part 2
(count (filter true? (map #(some true? (map safe? (concat [%] (remove-each %)))) data)))
