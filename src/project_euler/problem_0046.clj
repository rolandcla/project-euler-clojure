(ns project-euler.problem_0046)

;; It was proposed by Christian Goldbach that every odd composite number
;; can be written as the sum of a prime and twice a square.

;; 9 = 7 + 2×1^2
;; 15 = 7 + 2×2^2
;; 21 = 3 + 2×3^2
;; 25 = 7 + 2×3^2
;; 27 = 19 + 2×2^2
;; 33 = 31 + 2×1^2

;; It turns out that the conjecture was false.

;; What is the smallest odd composite that cannot be written
;; as the sum of a prime and twice a square?

;; from problem 0007 ----------------------------------
;;....................................................

(defn prime?
  [n]
  {:pre [(integer? n) (> n 0)]}
  (or (== 2 n)
      (let [mmax (+ (Math/sqrt n) 1)]
        (not (some #(== 0 (rem n %)) (concat [2] (range 3 mmax 2))))
        )))

(defn is-int-sqrt? [x]
  (let [y (int (+ 0.5 (Math/sqrt x)))]
    (== x (* y y))
    ))

(defn solution []
  (->> (iterate #(+ 2 %) 3)
       (remove prime?)
       (map (fn [c]
              (->> (cons 2  (iterate #(+ 2 %) 3))
                   (filter prime?)
                   (map #(- c %))
                   (filter even?)
                   (take-while #(> % 1))
                   (map #(/ % 2))
                   ((fn [x] (when (not (some is-int-sqrt? x)) c)))
                   )))
       (some identity)
       ))

(solution)

;;-> 5777

