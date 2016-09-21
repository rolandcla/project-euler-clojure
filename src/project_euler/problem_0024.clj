(ns project-euler.problem_0024)

;; A permutation is an ordered arrangement of objects.
;; For example, 3124 is one possible permutation of the digits 1, 2, 3 and 4.
;; If all of the permutations are listed numerically or alphabetically, we call it lexicographic order.
;; The lexicographic permutations of 0, 1 and 2 are:

;; 012   021   102   120   201   210

;; What is the millionth lexicographic permutation of the digits 0, 1, 2, 3, 4, 5, 6, 7, 8 and 9?


(defn swaps
  [s]
  (let [l (count s)]
    (for [i (range l)]
      (str (nth s i) (subs s 0 i) (subs s (inc i) l))))
  )

(defn permutations
  [s]
  (if (== 1 (count s))
    [s]
    (mapcat
     (fn [sw] (map #(str (first sw) %) (permutations (subs sw 1))))
     (swaps s)
     )
    ))

(defn solution []
  (nth (permutations "0123456789") 999999))

;;-> 2783915460


