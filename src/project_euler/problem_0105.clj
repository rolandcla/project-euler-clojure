(ns project-euler.problem_0105
  (:require [clojure.math.combinatorics :as combo]))

;; Let S(A) represent the sum of elements in set A of size n.
;; We shall call it a special sum set if for any two non-empty disjoint subsets, B and C,
;; the following properties are true:

;; S(B) ≠ S(C); that is, sums of subsets cannot be equal.
;; If B contains more elements than C then S(B) > S(C).
;; For example, {81, 88, 75, 42, 87, 84, 86, 65} is not a special sum set
;; because 65 + 87 + 88 = 75 + 81 + 84, whereas {157, 150, 164, 119, 79, 159, 161, 139, 158}
;; satisfies both rules for all possible subset pair combinations and S(A) = 1286.

;; Using sets.txt (right click and "Save Link/Target As..."),
;; a 4K text file with one-hundred sets containing seven to twelve elements
;; (the two examples given above are the first two sets in the file),
;; identify all the special sum sets, A1, A2, ..., Ak, and find the value of S(A1) + S(A2) + ... + S(Ak).

;; NOTE"" This problem is related to Problem 103 and Problem 106.

(defn pairs-of-ixs [n]
  (->> (range 2 (inc (/ n 2)))
       (mapcat (fn [m]
                 (->> (combo/combinations (range 1 (- n 1)) m)
                      (map vec)
                      (mapcat (fn [m-seq]
                                (->> (let [l-seq (combo/combinations (range (first m-seq)) 1)
                                           h-seq (combo/combinations (range (inc (last m-seq)) n) 1)]
                                       (->> (combo/cartesian-product l-seq h-seq)
                                            (map flatten)
                                            (mapcat (fn [[l h]]
                                                      (let [os (filter #(== -1 (.indexOf m-seq %))
                                                                       (range (inc l) h))]
                                                        (->> (combo/combinations os (- m 2))
                                                             (map (fn [ps]
                                                                    [m-seq (vec (concat  [l] ps [h]))]))
                                                             )
                                                        )))
                                            ))
                                     )))
                      ))))
  )

(def mz-pairs-of-ixs
  (memoize pairs-of-ixs))

(defn property-2? [xs]
  (let [cnt (count xs)]
      (->> (range 1 cnt)
           (remove (fn [n] (> (reduce + (take (inc n) xs))
                              (reduce + (drop (- cnt n) xs)))))
           (seq)
           (not)
           )))

(defn property-1? [xs]
  (->> (mz-pairs-of-ixs (count xs))
       (some (fn [[m-ixs o-ixs]]
               (== (reduce (fn [s ix] (+ s (nth xs ix))) 0 m-ixs)
                   (reduce (fn [s ix] (+ s (nth xs ix))) 0 o-ixs))))
       (not)
       ))

(defn solution []
  (->> (slurp "resources/p105_sets.txt")
       (clojure.string/split-lines)
       (map (fn [line] (vec (sort (map read-string (clojure.string/split line #","))))))
       (filter (fn [xs] (and (property-2? xs)
                             (property-1? xs))))
       (map (partial reduce +))
       (reduce +)
       ))

;;-> 73702
