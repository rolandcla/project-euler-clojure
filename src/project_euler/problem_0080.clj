(ns project-euler.problem_0080)

;; It is well known that if the square root of a natural number is not an integer, then it is irrational.
;; The decimal expansion of such square roots is infinite without any repeating pattern at all.

;; The square root of two is 1.41421356237309504880...,
;; and the digital sum of the first one hundred decimal digits is 475.

;; For the first one hundred natural numbers,
;; find the total of the digital sums of the first one hundred decimal digits for all the irrational square roots.

(defn one-digit-from-sqrt [x y step d]
  (let [y' (+ y step)
        d' (inc d)]
    (if (< (* y' y') x)
      (recur x y' step d')
      [y d])))

(defn sqrt-digits [x]
  (loop [y 0M, step 1M, ds [], n 0]
    (if (== n 100)
      ds
      (let [[y' d] (one-digit-from-sqrt x y step 0)]
        (recur y' (/ step 10) (conj ds d) (inc n)))
      )))

(defn solution []
  (->> (clojure.set/difference (set (for [i (range 100)] i)) (set (for [i (range 10)] (* i i))))
       (map (fn [x] (apply + (sqrt-digits x))))
       (apply +)
       ))

;;-> 40886
