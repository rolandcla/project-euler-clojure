(ns project-euler.problem-0132
  (:require [clojure.math.combinatorics :as combo]))

;; A number consisting entirely of ones is called a repunit. We shall define R(k) to be a repunit of length k.
;; For example, R(10) = 1111111111 = 11×41×271×9091, and the sum of these prime factors is 9414.
;; Find the sum of the first forty prime factors of R(10^9).

;; From problem 0010 :
;; -------------------
(defn prime?
  [n]
  {:pre [(integer? n) (> n 0)]}
  (or (== 2 n)
      (let [mmax (+ (Math/sqrt n) 1)]
        (not (some #(== 0 (rem n %)) (concat [2] (range 3 mmax 2))))
        )))
;;------------------------------------------------------------------------

(defn prime-factors [x]
  (letfn [(next-fact [n] (if (== n 2) 3 (+ n 2)))]
    (loop [x x, p 2, pfs []]
      (cond (== 1 x)          pfs
            (> (* p p) x)     (conj pfs x)
            (zero? (rem x p)) (recur (quot x p) p (conj pfs p))
            :else             (recur x (next-fact p) pfs)
            ))))


(def divisors-of-1e9
  (->> (prime-factors 1000000000)
       combo/subsets
       (map #(apply * 1 %))
       set
       ))

(defn a [n]
  (loop [r 1 k 1]
    (let [r' (rem r n)]
      (if (zero? r')
        k
        (recur (+ 1 (* 10 r')) (inc k))
        ))))

(defn solution []
  (->> (iterate inc 3)
       (remove #(zero? (rem % 5)))
       (filter prime?)
       (map (fn [n] [n (a n)]))
       (filter #(divisors-of-1e9 (second %)))
       (take 40)
       (map first)
       (apply +)
       ))

;;-> 843296
