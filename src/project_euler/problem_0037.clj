(ns project-euler.problem_0037)

;; The number 3797 has an interesting property.
;; Being prime itself, it is possible to continuously remove digits from left to right,
;; and remain prime at each stage: 3797, 797, 97, and 7.
;; Similarly we can work from right to left: 3797, 379, 37, and 3.

;; Find the sum of the only eleven primes that are both truncatable
;; from left to right and right to left.

;; NOTE: 2, 3, 5, and 7 are not considered to be truncatable primes.

;; from problem 0007 ----------------------------------
;;....................................................

(defn prime?
  [n]
  {:pre [(integer? n) (> n 0)]}
  (and (not (== 1 n))
       (or (== 2 n)
           (let [mmax (+ (Math/sqrt n) 1)]
             (not (some #(== 0 (rem n %)) (concat [2] (range 3 mmax 2))))
             ))))

;;....................................................................


(defn sub-numbers
  [n]
  (->> (iterate #(* 10 %) 10)
       (map (fn [d] [(quot n d) (rem n d)]))
       (take-while (fn [[q r]] (not (== 0 q))))
       (apply concat)
       (filter #(not (== 0 %)))
       ))

(defn solution []
  (->> (iterate #(+ 2 %) 11)
       (filter prime?)
       (filter #(every? prime? (sub-numbers %)))
       (take 11)
       (apply +)
       ))

(solution)

;;-> 748317

