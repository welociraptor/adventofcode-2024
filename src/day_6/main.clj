(ns day_6.main
  (:require [clojure.string :as str]))

(def input (->> "src/day_6/input" slurp str/split-lines (map vec) vec))

(defn get-in [vec [x y]] (str ((vec y) x)))

(defn map-direction [c]
  (let [dir {\^ 0 \> 1 \v 2 \< 3}]
    (dir c)))

(defn find [matrix matcher]
  (for [y (range (count matrix))
        x (range (count (first matrix)))
        c (get-in matrix [x y])
        :when (matcher (get-in matrix [x y]))]
    [x y (map-direction c)])
  )

; guard coordinates
(def guard (first (find input #(re-matches #"<|v|>|\^" %))))

(defn wall? [data x y] (= (get-in data [x y]) "#"))

(defn blocked? [data [x y d]]
  (case d
    0 (when (wall? data x (dec y)) true)
    1 (when (wall? data (inc x) y) true)
    2 (when (wall? data x (inc y)) true)
    3 (when (wall? data (dec x) y) true)
    )
  )

(defn exiting? [data [x y d]]
  (case d
    0 (when (= y 0) true)
    1 (when (= x (dec (count (first data)))) true)
    2 (when (= y (dec (count data))) true)
    3 (when (= x 0) true)
    )
  )

(defn next-dir [d] (if (= d 3) 0 (inc d)))

(defn move [[x y d]]
  (case d
    0 [x (dec y) d]
    1 [(inc x) y d]
    2 [x (inc y) d]
    3 [(dec x) y d]
    )
  )

(defn next-state [data [x y d]]
  (if (blocked? data [x y d]) [x y (next-dir d)]
                              (move [x y d])
                              )
  )

; solution for part 1
(loop [d input g guard v #{[(guard 0) (guard 1)]}]
  (if (exiting? d g) (count (conj v [(g 0) (g 1)]))
                     (recur d (next-state d g) (conj v [(g 0) (g 1)]))
                     )
  )

; ----- part 2 -----

(def path
  (loop [d input g guard v #{[(g 0) (g 1)]}]
    (if (exiting? d g) (conj v [(g 0) (g 1)])
                       (recur d (next-state d g) (conj v [(g 0) (g 1)]))
                       )
    )
  )

(defn obstruct [data [x y]] (assoc-in data [y x] \#))

; see if we have been already at this position facing this way
(defn deja-vu? [path state]
  (contains? path state)
  )

; if we end up in the same position, it's an eternal loop. if we exit the area, it's not
(defn eternal-loop? [data guard]
  (loop [d data g guard v #{}]
    (cond (exiting? d g) false
          (deja-vu? v g) true
          :else (recur d (next-state d g) (conj v g))
          )
    )
  )

(defn count-loops [data]
  (count
    (filter true?
            (reduce
              (fn [acc [x y]]
                (conj acc (eternal-loop? (obstruct data [x y]) guard))
                )
              []
              path
              )
            )))

; solution for part 2
(count-loops input)
