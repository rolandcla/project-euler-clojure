(ns project-euler.problem_0036
  (:require [clojure.pprint :refer [cl-format]]))

;; The decimal number, 585 = 10010010012 (binary), is palindromic in both bases.

;; Find the sum of all numbers, less than one million, which are palindromic in base 10 and base 2.

;; (Please note that the palindromic number, in either base, may not include leading zeros.)


(defn palindrome?
  [s]
  (let [l (count s)]
    (or (< l 2)
        (and  (= (get s 0) (get s (dec l)))
              (palindrome? (subs s 1 (dec l)))))
    ))


(defn solution []
  (->> (range 1000000)
       (filter #(and (palindrome? (str %))
                     (palindrome? (cl-format nil "~,'0',B" %))))
       (reduce +)
       ))

(solution)

;;-> 872187




