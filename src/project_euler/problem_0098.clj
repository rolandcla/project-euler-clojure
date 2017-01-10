(ns project-euler.problem_0098
  (:require [clojure.string :as str])
  (:require [clojure.math.combinatorics :as combo]))

;; By replacing each of the letters in the word CARE with 1, 2, 9, and 6 respectively,
;; we form a square number: 1296 = 362.
;; What is remarkable is that, by using the same digital substitutions, the anagram,
;; RACE, also forms a square number: 9216 = 962.
;; We shall call CARE (and RACE) a square anagram word pair
;; and specify further that leading zeroes are not permitted,
;; neither may a different letter have the same digital value as another letter.

;; Using words.txt (right click and 'Save Link/Target As...'),
;; a 16K text file containing nearly two-thousand common English words,
;; find all the square anagram word pairs (a palindromic word is NOT considered to be an anagram of itself).

;; What is the largest square number formed by any member of such a pair?

;; NOTE: All anagrams formed must be contained in the given text file.

;;-------------------------------------------------------------------------------------------------------
;; from problem_0030:

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
;;-------------------------------------------------------------------------------------------------------

(defn read-anagrams []
  (->> (str/split (slurp "resources/p098_words.txt") #",")
       (map (fn [s] (butlast (next s))))
       (group-by sort)
       (filter (fn [[k v]] (> (count v) 1)))
       (sort-by (comp count first) >)
       (map second)
       ))

(defn make-anagram-pattern [xs ys]
  (letfn [(positions [vs]
            (->> (map-indexed (fn [ix v] [ix v]) vs)
                 (group-by second)
                 (map (fn [[v ixs]] [v (set (map first ixs))]))
                 (into {})
                 ))]
    (let [pos-xs (positions xs)]
      (->> (seq (positions ys))
           (map (fn [[y y-ixs]] [(pos-xs y) y-ixs]))
           (set)
           ))))

(defn make-patterns [anagrams]
  (->> anagrams
       (mapcat (fn [anag] (combo/combinations anag 2)))
       (mapcat (fn [[xs ys]] [(make-anagram-pattern xs ys)
                              (make-anagram-pattern ys xs)]))
       ))



(defn solution []
  (let [anagrams (read-anagrams)
        n (count (ffirst anagrams))
        word-patterns (->> anagrams
                           (make-patterns)
                           (set)
                           )]
    (->> (loop [x (int (Math/sqrt (Math/pow 10 n)))
                sq-map {}]
           (let [x2 (* x x)
                 ds (digits x2)
                 sds (sort ds)
                 ]
             (if-let [[_ y2s] (find sq-map sds)]
               (or (->> y2s
                        (map (fn [y2] [y2 (digits y2)]))
                        (map (fn [[y2 y2ds]] [x2 y2 (make-anagram-pattern ds y2ds)]))
                        (filter (fn [[_ _ pat]] (word-patterns pat)))
                        (first)
                        )
                   (recur (dec x) (update sq-map sds #(conj % x2))))
               (recur (dec x) (assoc sq-map sds [x2]))
               )))
         ;;(first)
         )))
