(ns project-euler.problem_0127)

;; The radical of n, rad(n), is the product of distinct prime factors of n.
;; For example, 504 = 2^3 × 3^2 × 7, so rad(504) = 2 × 3 × 7 = 42.

;; We shall define the triplet of positive integers (a, b, c) to be an abc-hit if:

;; GCD(a, b) = GCD(a, c) = GCD(b, c) = 1
;; a < b
;; a + b = c
;; rad(abc) < c
;; For example, (5, 27, 32) is an abc-hit, because:

;; GCD(5, 27) = GCD(5, 32) = GCD(27, 32) = 1
;; 5 < 27
;; 5 + 27 = 32
;; rad(4320) = 30 < 32
;; It turns out that abc-hits are quite rare and there are only thirty-one abc-hits for c < 1000,
;; with ∑c = 12523.

;; Find ∑c for c < 120000.

(defn gcd [x y]
  (if (zero? y)
    x
    (recur y (rem x y))))

(defn rad [x]
  (loop [x x p 2 q 0 r 1]
    (cond (> (* p p) x)   (if (== x q) r (* r x))
          (< 0 (rem x p)) (recur x          (if (== 2 p) 3 (inc p)) p r)
          (== p q)        (recur (quot x p) p                       p r)
          :else           (recur (quot x p) p                       p (* r p))
        )))

(defn naive-solution-for [c-max]
  (->> (range 3 c-max)
       (mapcat (fn [c]
                 (->> (if (odd? c) (iterate inc 1) (iterate #(+ % 2) 1))
                      (map (fn [a] [a (- c a) c]))
                      (take-while (fn [[a b c]] (< a b)))
                      )))
       (filter (fn [[a b c]] (and (== 1 (gcd a b))
                                  (== 1 (gcd a c))
                                  ;;(== 1 (gcd b c))
                                  (< (rad (* a b c)) c)
                                  )))
       (map (fn [[a b c]] c))
       (reduce +)
       ;;(count)
       ))
