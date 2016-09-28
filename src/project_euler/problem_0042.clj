(ns project-euler.problem_0042
  (:require [clojure.string :as str]))

;; The nth term of the sequence of triangle numbers is given by, tn = ½n(n+1);
;; so the first ten triangle numbers are:

;; 1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...

;; By converting each letter in a word to a number corresponding to
;; its alphabetical position and adding these values we form a word value.
;; For example, the word value for SKY is 19 + 11 + 25 = 55 = t10.
;; If the word value is a triangle number then we shall call the word a triangle word.

;; Using words.txt (right click and 'Save Link/Target As...'),
;; a 16K text file containing nearly two-thousand common English words, how many are triangle words?

(defn triangle-seq []
  (->> (iterate inc 1)
       (map (fn [n] (/ (* n (inc n)) 2)))
       ))

(defn is-a-triangle-number?
  [n]
  (loop [tr-seq (triangle-seq)]
    (let [[x & new-tr-seq] tr-seq]
      (or (= x n)
          (and (< x n)
               (recur new-tr-seq))))))

(defn word-value
  [word]
  (apply + (map (fn [c] (- (int  c) 64)) word)))

(def all-words
  (->> (slurp "resources/p042_words.txt")
       (#(str/split % #","))
       (map #(subs % 1 (dec (count %))))
       )
  )

(defn solution []
  (->> all-words
       (map word-value)
       (filter is-a-triangle-number?)
       (count)
       ))

(solution)

;;-> 162

