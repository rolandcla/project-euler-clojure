(ns project-euler.problem_0077)

;; It is possible to write ten as the sum of primes in exactly five different ways:

;; 7 + 3
;; 5 + 5
;; 5 + 3 + 2
;; 3 + 3 + 2 + 2
;; 2 + 2 + 2 + 2 + 2

;; What is the first value which can be written as the sum of primes in over five thousand different ways?

;; From problem 0010 :
;; ---------------------
(defn prime?
  [n]
  {:pre [(integer? n) (> n 0)]}
  (or (== 2 n)
      (let [mmax (+ (Math/sqrt n) 1)]
        (not (some #(== 0 (rem n %)) (concat [2] (range 3 mmax 2))))
        )))

;;==========================================================================

(defn primes-under [x]
  (filter prime? (range 2 x)))

(def ways-to-sum
  (memoize
   (fn  [sum [x & xs :as xss]]
     ;;(println sum xss)
     (cond
       (zero? sum) 1
       (not xs)    (if (zero? (rem sum x)) 1 0)
       (> x sum)   (ways-to-sum sum xs)
       :else       (+ (ways-to-sum (- sum x) xss) (ways-to-sum sum xs))
       ))))


(defn solution []
  (->> (iterate inc 10)
       (some (fn [x] (and (< 5000 (ways-to-sum x (primes-under x))) x)))))
