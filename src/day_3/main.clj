(ns day_3.main
  (:require [clojure.string :as str]))

; solution for part 1
(reduce + (map #(* (Integer/parseInt (% 1)) (Integer/parseInt (% 2))) (re-seq #"mul\((\d+),(\d+)\)" (slurp "src/day_3/input"))))

; solution for part 2
(:sum
  (reduce
    (fn [acc i]
      (cond
        (= (i 0) "do()") {:active true :sum (:sum acc)}
        (= (i 0) "don't()") {:active false :sum (:sum acc)}
        :else (case (:active acc) true {:active true :sum (+ (:sum acc) (* (Integer/parseInt (i 2)) (Integer/parseInt (i 3))))}
                                  false {:active false :sum (:sum acc)})
        )
      )
    {:active true :sum 0}
    (vec (re-seq #"(do\(\)|don't\(\)|mul\((\d+),(\d+)\))" (slurp "src/day_3/input")))
    ))
