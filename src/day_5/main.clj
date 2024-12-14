(ns day_5.main
  (:require [clojure.string :as str]))

(defn to-int [x] (Integer/parseInt x))

(defn split-rule [in] (vec (map to-int (str/split in #"\|"))))

(def data (str/split-lines (slurp "src/day_5/input")))

(def rules
  (reduce
    (fn [acc [x y]] (assoc acc x (set (conj (acc x) y))))
    {}
    (map split-rule (subvec data 0 (.indexOf data "")))
    ))

(def updates (map #(mapv to-int %) (map #(str/split % #",") (subvec data (inc (.indexOf data ""))))))

(defn contains-any? [set1 set2]
  (some #(contains? set1 %) set2))

(defn no-violation? [v]
  (for [i (range 1 (count v))]
    (contains-any? (rules (v i)) (set (take i v)))
    )
  )

(defn ok? [x] (every? nil? (no-violation? x)))

(defn middle-element [coll]
  (let [seq-coll (seq coll)
        mid-index (quot (count seq-coll) 2)]
    (nth seq-coll mid-index)))

; solution for part 1
(reduce
  (fn [acc x] (if (ok? x) (+ acc (middle-element x)) acc))
  0
  updates
  )

; ------ part 2 -----

(def ordering-rules (map split-rule (subvec data 0 (.indexOf data ""))))

(defn index-of [vec a] (when (some #{a} vec) (.indexOf vec a)))

(defn move-element [v from to]
  (let [elem (nth v from)
        without-elem (vec (concat (subvec v 0 from) (subvec v (inc from))))]
    (vec (concat (subvec without-elem 0 to) [elem] (subvec without-elem to)))))

(defn reorder [vec]
  (reduce
    (fn [acc [a b]]
      (let [i (index-of acc a)
            j (index-of acc b)]
        (if (and (not (nil? i))
                 (not (nil? j))
                 (> i j)
                 ) (move-element acc i j) acc
                   )))
    vec
    ordering-rules
    )
  )

(defn repeat-until [pred f x]
  (let [result (f x)]
    (if (pred result)
      result
      (recur pred f result))))

(def fixed-updates (map #(repeat-until ok? reorder %) (remove ok? updates)))

; solution for part 2
(reduce
  (fn [acc x]
    (+ acc (middle-element x))
    )
  0
  fixed-updates)
