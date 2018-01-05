(ns project-euler.problem_0145)

;; Some positive integers n have the property that the sum [ n + reverse(n) ] consists
;; entirely of odd (decimal) digits.
;; For instance, 36 + 63 = 99 and 409 + 904 = 1313.
;; We will call such numbers reversible; so 36, 63, 409, and 904 are reversible.
;; Leading zeroes are not allowed in either n or reverse(n).

;; There are 120 reversible numbers below one-thousand.

;; How many reversible numbers are there below one-billion (10^9)?

;; ==========================================================================

;; A: [2 4 6 8] + [1 3 5 7]     < 10 ==> 20 pairs
;; B: [2 4 6 8] + [1 3 5 7 9]   > 10 ==> 20 pairs
;; C: [0 1 2 3 4]                    ==> 5
;; D: [0 2 4 6 8] + [1 3 5 7 9] < 10 ==> 30 pairs
;; E: [0 2 4 6 8] + [1 3 5 7 9] > 10 ==> 20 pairs


;; 2 digits : AA             => 20
;; 3 digits : BCB            => 5 * 20 = 100
;; 4 digits : ADDA           => 20 * 30 = 600
;; 5 digits                  => 0
;; 6 digits : ADDDDA         => 20 * 30 * 30 = 18000
;; 7 digits : BCECECB        => 20 * 5 * 20 * 5 * 5 = 50000
;; 8 digits : ADDDDDDA       => 20 * 30 * 30 * 30 = 540000

(defn solution  []
  (+ 20
     100
     600
     18000
     50000
     540000))

;; 608720
