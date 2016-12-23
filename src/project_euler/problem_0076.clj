(ns project-euler.problem_0076
  (:require [clojure.math.combinatorics :as combo]))

;; It is possible to write five as a sum in exactly six different ways:

;; 4 + 1
;; 3 + 2
;; 3 + 1 + 1
;; 2 + 2 + 1
;; 2 + 1 + 1 + 1
;; 1 + 1 + 1 + 1 + 1

;; How many different ways can one hundred be written as a sum of at least two positive integers?

(defn sum-generator [n]
  (let [ones (repeat n 1)]
    (->> (combo/partitions ones :min 2)
         (map (fn [s] (map count s)))
         )))

(defn start-histo [n]
  (->> (sum-generator n)
       (group-by first)
       (map (fn [[beg sums]] [beg (count sums)]))
       (sort)
       ))

(defn full-histo [m]
  (->> (range 2 m)
       (map (fn [n] [n (start-histo n)]))
       (map (fn [[n h]] (println (format "%2d" n) (map #(format "%-7s" %) h))))
       ))


  (defn naive-n-of-sums [n]
    (count (sum-generator count)))

