(ns project-euler.problem_0094)

;; It is easily proved that no equilateral triangle exists with integral length sides and integral area.
;; However, the almost equilateral triangle 5-5-6 has an area of 12 square units.

;; We shall define an almost equilateral triangle to be a triangle for which two sides are equal
;; and the third differs by no more than one unit.

;; Find the sum of the perimeters of all almost equilateral triangles with integral side lengths
;; and area and whose perimeters do not exceed one billion (1,000,000,000).

(defn r1dot [a b c]
  [
   (+ a       (* -2 b) (* 2 c))
   (+ (* 2 a) (- b)    (* 2 c))
   (+ (* 2 a) (* -2 b) (* 3 c))
   ])

(defn r2dot [a b c]
  [
   (+ a       (* 2 b) (* 2 c))
   (+ (* 2 a) b       (* 2 c))
   (+ (* 2 a) (* 2 b) (* 3 c))
   ])

(defn r3dot [a b c]
  [
   (+ (- a)    (* 2 b) (* 2 c))
   (+ (* -2 a) b       (* 2 c))
   (+ (* -2 a) (* 2 b) (* 3 c))
   ])

;; Brute-force !!!

(defn rec-triangle [a b c]
  (lazy-seq
   (if (< c 500000000)
     (let [a2 (* 2 a)
           b2 (* 2 b)
           d1 (- c a2)
           d2 (- c b2)]
       (cond (or (== 1 d1) (== -1 d1)) (cons [a2 c] (concat (apply rec-triangle (r1dot a b c))
                                                            (apply rec-triangle (r2dot a b c))
                                                            (apply rec-triangle (r3dot a b c))
                                                            ))
             (or (== 1 d2) (== -1 d2)) (cons [b2 c] (concat (apply rec-triangle (r1dot a b c))
                                                            (apply rec-triangle (r2dot a b c))
                                                            (apply rec-triangle (r3dot a b c))
                                                            ))
             :else                                     (concat (apply rec-triangle (r1dot a b c))
                                                               (apply rec-triangle (r2dot a b c))
                                                               (apply rec-triangle (r3dot a b c))
                                                               )))
     nil))
  )

(def b-triangles [[6 5] [16 17] [66 65] [240 241] [902 901] [3360 3361] [12546 12545]
                  [46816 46817] [174726 174725] [652080 652081] [2433602 2433601]
                  [9082320 9082321] [33895686 33895685] [126500416 126500417] [472105986 472105985]])

(defn solution []
  (->> b-triangles
       ;;(rec-triangle 3 4 5) ;; Looong.... and option ':jvm-opts ["-Xss10M"]' needed in project.clj
       (map (fn [[a b]] (+ a b b)))
       (filter (fn [perim] (< perim 1e9)))
       (reduce +)
       ))

;;-> 518408346
