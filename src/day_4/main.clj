(ns day_4.main
  (:require [clojure.string :as str]))

(def matrix (vec (map vec (-> "src/day_4/input" slurp str/split-lines vec))))

(defn get-diagonals [matrix]
  (let [n (count matrix)
        m (count (first matrix))]
    (for [i (range (+ n m -1))]
      (for [j (range (max 0 (- i (- m 1))) (min n (inc i)))]
        (get-in matrix [j (- i j)])))))

(defn get-anti-diagonals [matrix]
  (let [n (count matrix)
        m (count (first matrix))]
    (for [i (range (+ n m -1))]
      (for [j (range (max 0 (- i (- n 1))) (min m (inc i)))]
        (get-in matrix [(- n 1 (- i j)) j])))))

(defn transpose-90 [matrix] (apply mapv vector matrix))

(def t0 (map #(apply str %) matrix))
(def t-45 (map #(apply str %) (get-diagonals matrix)))
(def t45 (map #(apply str %) (get-anti-diagonals matrix)))
(def t90 (map #(apply str %) (transpose-90 matrix)))

(defn count-xmas [s] (+ (count (re-seq #"XMAS" s)) (count (re-seq #"SAMX" s))))

(def data (concat t0 t-45 t45 t90))

; solution for part 1
(println (reduce (fn [acc x] (+ acc (count-xmas x))) 0 data))

(defn loc [x y] ((matrix x) y))

(def n (- (count matrix) 1))

(def coordinates (for [x (range 1 n) y (range 1 n)] [x y]))

(defn is-mas? [[x y]]
  (and
    (= (loc x y) \A)
    (or
      (and (= (loc (dec x) (dec y)) \M)
           (= (loc (inc x) (inc y)) \S))
      (and (= (loc (dec x) (dec y)) \S)
           (= (loc (inc x) (inc y)) \M))
      )
    (or
      (and (= (loc (dec x) (inc y)) \M)
           (= (loc (inc x) (dec y)) \S))
      (and (= (loc (dec x) (inc y)) \S)
           (= (loc (inc x) (dec y)) \M))
      )
    )
  )

; solution for part 2
(reduce (fn [acc [x y]] (if (is-mas? [x y]) (inc acc) acc)) 0 coordinates)