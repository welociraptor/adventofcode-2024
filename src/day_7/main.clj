(ns day_7.main
  (:require [clojure.string :as str]))

(defn parse [in] (map #(bigint %) (re-seq #"\d+" in)))

(def input (->> "src/day_7/input"
                slurp
                str/split-lines
                (map parse)))

; create all possible combinations of operations for a vector length of n
(defn ops-combinations [n]
  (if (zero? n)
    [[]]
    (for [x [+ *]
          xs (ops-combinations (dec n))]
      (cons x xs))))

; perform a single calculation with vectors of operations and operands
(defn calc [ops nums]
  (loop [acc (first nums)
         ops ops
         nums (rest nums)]
    (if (empty? ops) acc
                     (recur ((first ops) acc (first nums)) (rest ops) (rest nums)))))

; calculate all possible permutations
(defn calc-all [nums]
  (map #(calc % nums) (ops-combinations (dec (count nums)))))

; is the calculation viable?
(defn viable? [in] (some #(= % (first in)) (calc-all (rest in))))

; solution for part 1
(reduce
  (fn [acc in]
    (+ acc (first in))
    )
  0
  (filter viable? input))
