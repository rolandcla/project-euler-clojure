(ns project-euler.problem_0004)

;; A palindromic number reads the same both ways.
;; The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.

;; Find the largest palindrome made from the product of two 3-digit numbers.

(require '[clojure.string :as s])

(defn is-palindrome?
  [x]
  (= x (s/reverse x)))

(defn palindromes-from-3d-numbers
  []
  (filter #(is-palindrome? (str %))
          (for [x (range 100 1000)
                y (range 100 1000)]
            (* x y))))

(defn solution []
  (apply max (palindromes-from-3d-numbers)))

;;-> 906609

