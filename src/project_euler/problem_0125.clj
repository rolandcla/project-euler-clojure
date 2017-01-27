(ns project-euler.problem_0125)

;; The palindromic number 595 is interesting because it can be written
;; as the sum of consecutive squares: 6^2 + 7^2 + 8^2 + 9^2 + 10^2 + 11^2 + 12^2.

;; There are exactly eleven palindromes below one-thousand that can be written
;; as consecutive square sums, and the sum of these palindromes is 4164.
;; Note that 1 = 0^2 + 1^2 has not been included as this problem is
;; concerned with the squares of positive integers.

;; Find the sum of all the numbers less than 10^8 that are both palindromic
;; and can be written as the sum of consecutive squares.

(defn digits [x]
  (loop [x x ds []]
    (if (zero? x)
      ds
      (recur (quot x 10) (conj ds (rem x 10))))))

(defn palindromic? [x]
  (let [ds (digits x)]
    (= ds (rseq ds))))

(defn palindromic-cons-squares-from [a m]
  (loop [a  a
         s  (* a a)
         ps []]
    (let [a' (inc a)
          s' (+ s (* a' a'))]
      (if (< s' m)
        (recur a'
               s'
               (if (palindromic? s') (conj ps s') ps))
        ps)
      )))

(defn palindromic-cons-squares [m]
  (->> (range 1 (Math/sqrt m))
       (mapcat (fn [a] (palindromic-cons-squares-from a m)))
       (set)
       ))

(defn solution []
  (->> (palindromic-cons-squares 100000000)
       (reduce +)
       ))

;;-> 2906969179
