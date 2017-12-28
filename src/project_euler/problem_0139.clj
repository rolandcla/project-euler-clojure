(ns project-euler.problem_0139)

;; Let (a, b, c) represent the three sides of a right angle triangle with integral length sides.
;; It is possible to place four such triangles together to form a square with length c.

;; For example, (3, 4, 5) triangles can be placed together to form a 5 by 5 square with a 1 by 1 hole in the middle
;; and it can be seen that the 5 by 5 square can be tiled with twenty-five 1 by 1 squares.

;; However, if (5, 12, 13) triangles were used then the hole would measure 7 by 7
;; and these could not be used to tile the 13 by 13 square.

;; Given that the perimeter of the right triangle is less than one-hundred million,
;; how many Pythagorean triangles would allow such a tiling to take place?

;;===============================================================================================

;; from wikipedia https://en.wikipedia.org/wiki/Tree_of_primitive_Pythagorean_triples

(defn ppt-children [[a b c]]
  [(let [a-2b+2c (- (+ a c c) b b)]
     [a-2b+2c
      (+ a-2b+2c a b)
      (+ a-2b+2c a c)])
   (let [a+b+2c (+ a b c c)]
     [(+ a+b+2c b)
      (+ a+b+2c a)
      (+ a+b+2c a b c)])
   (let [b+2c-2a (- (+ b c c) a a)]
     [(+ b+2c-2a a b)
      b+2c-2a
      (+ b+2c-2a b c)])])

(defn ppt-lower-than [max-perim]
  (letfn [(rec-ppt [abc]
            (when (< (apply + abc) max-perim)
              (cons abc (mapcat rec-ppt (ppt-children abc)))
              ))]
    (rec-ppt [3 4 5])))

(defn count-gen []
  (->> (iterate (fn [abc] (nth (ppt-children abc) 2)) [3 4 5])
       (take-while (fn [abc] (< (apply + abc) 100000000)))
       count))

;; Après qq essais, il semble que seule la seconde matrice génère les triangles voulus

(defn solution-for [max-perim]
  (->> [3 4 5]
       (iterate (fn [abc] (second (ppt-children abc))))
       (take-while (fn [abc] (< (apply + abc) max-perim)))
       (map sort)
       (filter (fn [[a b c]] (zero? (mod c (- b a)))))
       (map (fn [abc] (quot (dec max-perim) (apply + abc))))
       (apply +)))

(defn solution []
  (solution-for 100000000))

;; 10057761
