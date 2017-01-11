(ns project-euler.problem_0099)

;; Comparing two numbers written in index form like 211 and 37 is not difficult,
;; as any calculator would confirm that 211 = 2048 < 37 = 2187.

;; However, confirming that 63238^2518061 > 519432^525806 would be much more difficult,
;; as both numbers contain over three million digits.

;; Using base_exp.txt (right click and 'Save Link/Target As...'),
;; a 22K text file containing one thousand lines with a base/exponent pair on each line,
;; determine which line number has the greatest numerical value.

;; NOTE: The first two lines in the file represent the numbers in the example given above.

(defn solution []
  (->> (slurp "resources/p099_base_exp.txt")
       (clojure.string/split-lines)
       (map-indexed (fn [ix line]
                      (->> (clojure.string/split line #",")
                           (map read-string)
                           ((fn [[base exp]] [(* (Math/log10 base) exp) ix]))
                           )))
       (sort-by first  >)
       (first)
       (second)
       (inc)
       ))

;;-> 709
