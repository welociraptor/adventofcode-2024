(ns day_8.main
  (:require [clojure.string :as str]))

(def input (->> "src/day_8/input" slurp str/split-lines (map vec) vec))

(def width (count (first input)))
(def height (count input))

(defn get-in [vec [x y]] (str ((vec y) x)))

; scan towers on the map
(defn towers [matrix]
  (for [y (range height)
        x (range width)
        f (get-in matrix [x y])
        :when (not= \. f)]
    [x y f]))

; get a set of frequencies
(defn list-frequencies [towers]
  (reduce
    (fn [acc [_ _ f]] (conj acc f))
    #{}
    towers))

(defn antinodes [[[x1 y1] [x2 y2]]]
  (let [dx (- x2 x1)
        dy (- y2 y1)]
    [[(- x1 dx) (- y1 dy)] [(+ x2 dx) (+ y2 dy)]]))

; make a map where antennas of same frequencies are listed under one key
(def antenna-map
  (reduce
    (fn [acc [x y f]]
      (assoc acc f (conj (acc f) [x y]))
      )
    {}
    (towers input)))

(defn antenna-pairs [freq]
  (->> (for [x (antenna-map freq)
             y (antenna-map freq)]
         (when-not (= x y) [x y]))
       (remove nil?)
       set))

(defn in-bounds? [[x y]]
  (and (>= x 0) (< x width)
       (>= y 0) (< y height)))

(def antinode-set
  (set (reduce
         (fn [acc in]
           (concat acc (filter in-bounds? (apply concat (mapv antinodes (antenna-pairs in)))))
           )
         []
         (list-frequencies (towers input)))))

; solution for part 1
(count antinode-set)

(defn interpolate [x y dx dy op]
  (loop [acc [[x y]]
         dx dx
         dy dy
         x (op x dx)
         y (op y dy)]
    (if (in-bounds? [x y]) (recur (conj acc [x y]) dx dy (op x dx) (op y dy)) acc)))

(defn resonant-antinodes [[[x1 y1] [x2 y2]]]
  (let [dx (- x2 x1)
        dy (- y2 y1)]
    (concat (interpolate x1 y1 dx dy -) (interpolate x2 y2 dx dy +))))

(def resonant-antinode-set
  (set (reduce
         (fn [acc in]
           (concat acc (filter in-bounds? (apply concat (mapv resonant-antinodes (antenna-pairs in)))))
           )
         []
         (list-frequencies (towers input)))))

; solution for part 2
(count resonant-antinode-set)
