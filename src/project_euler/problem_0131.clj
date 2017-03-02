(ns project-euler.problem_0131)

;; There are some prime values, p, for which there exists a positive integer, n,
;; such that the expression n^3 + n^2p is a perfect cube.

;; For example, when p = 19, 8^3 + 8^2×19 = 12^3.

;; What is perhaps most surprising is that for each prime with this property
;; the value of n is unique, and there are only four such primes below one-hundred.

;; How many primes below one million have this remarkable property?

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

(defn cube [x] (* x x x))

(defn solution []
  (->> (iterate inc 1)
       (map (fn [x] (- (cube (inc x)) (cube x))))
       (filter prime?)
       (take-while (fn [p] (< p 1000000)))
       (count)
       ))

;;-> 173
