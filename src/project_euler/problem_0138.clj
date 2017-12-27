(ns project-euler.problem_0138)

;; Consider the isosceles triangle with base length, b = 16, and legs, L = 17.

;; By using the Pythagorean theorem it can be seen that the height of the triangle,
;; h = √(172 − 82) = 15, which is one less than the base length.

;; With b = 272 and L = 305, we get h = 273, which is one more than the base length,
;; and this is the second smallest isosceles triangle with the property that h = b ± 1.

;; Find ∑ L for the twelve smallest isosceles triangles for which h = b ± 1 and b, L are positive integers.

;;========================================================================================================

(defn int-sqrt? [x]
  (let [sqrt-x (Math/round (Math/sqrt x))]
    (== x (*' sqrt-x sqrt-x))))

(defn solution-from [hb]
  (->> (iterate inc hb)
       (mapcat (fn [hb] (let [b (* hb 2)]
                          [(+ (* hb hb) (* (dec b) (dec b)))
                           (+ (* hb hb) (* (inc b) (inc b)))])))
       (filter int-sqrt?)
       (map (fn [h2] (int (Math/round (Math/sqrt h2)))))
       first))

(defn solution' []
  (loop [hb 8
         n 0
         ls []
         prev-l 1]
    (if (== n 7)
      ls
      (let [l (solution-from hb )]
        (recur (bigint (/ (*' hb l) prev-l))
               (inc n)
               (conj ls l)
               l
               ))
      )))

;; After observation of the FIRST solutions:

(defn solution []
  (loop [k 1
         l 17
         n 0
         ls []]
    (if (== 12 n)
      (apply + ls)
      (let [m (-' (*' 18 l) k)]
        (recur l
               m
               (inc n)
               (conj ls l))))))

;; 1118049290473932
