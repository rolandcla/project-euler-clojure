(ns project-euler.problem_0114
  (:require [clojure.math.combinatorics :as combo]))

;; A row measuring seven units in length has red blocks with a minimum length of three units placed on it,
;; such that any two red blocks (which are allowed to be different lengths)
;; are separated by at least one black square.
;; There are exactly seventeen ways of doing this.

;; How many ways can a row measuring fifty units in length be filled?

;; NOTE: Although the example above does not lend itself to the possibility,
;; in general it is permitted to mix block sizes.
;; For example, on a row measuring eight units in length you could use red (3), black (1), and red (4).


(defn compositions [n m]
  (if (< n m)
    [[]]
    (concat (compositions n (inc m))
            (map (fn [cs] (conj cs m))
                 (compositions (- n m 1) m))
            )))

(def mz-count-positions
  (memoize
   (fn [n blks]
     (if (empty? blks)
       1
       (let [nb (count blks)
             sum (reduce + blks)
             [b & bs] blks]
         (->> (range (- n sum nb -2))
              (map (fn [i] (mz-count-positions (- n b i 1) bs)))
              (reduce +')
              ))))))


(defn count-ways [n m]
  (->> (compositions n m)
       (map (fn [blks]
              (*' (combo/count-permutations blks)
                 (mz-count-positions n blks))))
       (reduce +')))

(defn solution []
  (count-ways 50 3))

;;-> 16475640049
