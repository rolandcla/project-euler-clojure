(ns project-euler.problem_0074)

;; The number 145 is well known for the property that the sum of the factorial of its digits is equal to 145:

;; 1! + 4! + 5! = 1 + 24 + 120 = 145

;; Perhaps less well known is 169, in that it produces the longest chain of numbers that link back to 169;
;; it turns out that there are only three such loops that exist:

;; 169 → 363601 → 1454 → 169
;; 871 → 45361 → 871
;; 872 → 45362 → 872

;; It is not difficult to prove that EVERY starting number will eventually get stuck in a loop. For example,

;; 69 → 363600 → 1454 → 169 → 363601 (→ 1454)
;; 78 → 45360 → 871 → 45361 (→ 871)
;; 540 → 145 (→ 145)

;; Starting with 69 produces a chain of five non-repeating terms,
;; but the longest non-repeating chain with a starting number below one million is sixty terms.

;; How many chains, with a starting number below one million, contain exactly sixty non-repeating terms?


;; from problem_0030:
;;---------------------------------------------------------------------------------
(defn digits
  [n]
  (loop [n n, ds []]
    (let [r (rem n 10)
          q (quot n 10)
          new-ds (conj ds r)]
      (if (== q 0)
        new-ds
        (recur q new-ds)
        ))))
;;------------------------------------------------------------------------------

(def fact [1 1 2 6 24 120 720 5040 40320 362880])

(defn sum-of-digit-fact [n]
  (apply + (map fact (digits n))))

(defn length-before-loop [n]
  (loop [n n, seen #{}, cnt 0]
    (if (seen n)
      cnt
      (recur (sum-of-digit-fact n) (conj seen n) (inc cnt)))))

(defn brute-force-solution []
  (->> (range 1 1000000)
       (map length-before-loop)
       (filter #(== 60 %))
       (count)
       ))

(def solution brute-force-solution)

;;-> 402
