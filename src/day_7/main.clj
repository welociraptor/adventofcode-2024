(ns day_7.main
  (:require [clojure.string :as str]))

(defn parse [in]
  (map #(bigint %) (re-seq #"\d+" in)))

(def input (->> "src/day_7/input"
                slurp
                str/split-lines
                (map parse)))

; create all possible combinations of operations for a vector length of n
(defn ops-combinations [ops n]
  (if (zero? n)
    [[]]
    (for [x ops
          xs (ops-combinations ops (dec n))]
      (cons x xs))))

; perform a single calculation with vectors of operations and operands
(defn calc [ops nums]
  (loop [acc (first nums)
         ops ops
         nums (rest nums)]
    (if (empty? ops) acc
                     (recur ((first ops) acc (first nums)) (rest ops) (rest nums)))))

; calculate all possible permutations
(defn calc-all [ops nums]
  (map #(calc % nums) (ops-combinations ops (dec (count nums)))))

; get a viability predicate for specified ops
(defn viablefn [ops]
  (fn [in] (some #(= % (first in)) (calc-all ops (rest in)))))

(defn solve [ops]
  (reduce
    (fn [acc in]
      (+ acc (first in))
      )
    0
    (filter (viablefn ops) input)))

; solution for part 1
(println (solve [+ *]))

(defn || [a b]
  (bigint (str a b)))

; solution for part 2
(println (solve [+ * ||]))
