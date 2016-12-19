(ns project-euler.problem_0062)

;; The cube, 41063625 (345^3), can be permuted to produce two other cubes: 56623104 (384^3) and 66430125 (405^3).
;; In fact, 41063625 is the smallest cube which has exactly three permutations of its digits which are also cube.

;; Find the smallest cube for which exactly five permutations of its digits are cube.

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


(defn cube [x] (* x x x))


(defn cubes-by-n-digits
  ([] (cubes-by-n-digits 10 10000))
  ([x lim]
   (lazy-seq
    (loop [x x, n-digit-cubes nil]
      (let [c (cube x)]
        (if (< c lim)
          (recur (inc x) (conj n-digit-cubes c))
          (cons n-digit-cubes (cubes-by-n-digits x (* 10 lim)))))))))

(defn n-digits-solution [cubes]
  (->> (group-by (comp sort vec digits) cubes)
       (filter (fn [[ds cs]] (== 5 (count cs))))))

(defn solution []
  (->> (cubes-by-n-digits)
       (map n-digits-solution)
       (some seq)
       (mapcat second)
       (apply min)
       ))

;;-> 127035954683

