(ns project-euler.problem-0096
  (:use [clojure.set :only (union difference)]))

;; Su Doku (Japanese meaning number place) is the name given to a popular puzzle concept.
;; Its origin is unclear, but credit must be attributed to Leonhard Euler who invented a similar,
;; and much more difficult, puzzle idea called Latin Squares.
;; The objective of Su Doku puzzles, however,
;; is to replace the blanks (or zeros) in a 9 by 9 grid in such that each row, column,
;; and 3 by 3 box contains each of the digits 1 to 9.
;; Below is an example of a typical starting puzzle grid and its solution grid.

;; A well constructed Su Doku puzzle has a unique solution and can be solved by logic,
;; although it may be necessary to employ "guess and test" methods in order to eliminate options
;; (there is much contested opinion over this).
;; The complexity of the search determines the difficulty of the puzzle;
;; the example above is considered easy because it can be solved by straight forward direct deduction.

;; The 6K text file, sudoku.txt (right click and 'Save Link/Target As...'),
;; contains fifty different Su Doku puzzles ranging in difficulty,
;; but all with unique solutions (the first puzzle in the file is the example above).

;; By solving all fifty puzzles find the sum of the 3-digit numbers found
;; in the top left corner of each solution grid; for example,
;; 483 is the 3-digit number found in the top left corner of the solution grid above.

(def test-grid
  [[11 12 13 14 15 16 17 18 18]
   [21 22 23 24 25 26 27 28 29]
   [31 32 33 34 35 36 37 38 39]
   [41 42 43 44 45 46 47 48 49]
   [51 52 53 54 55 56 57 58 59]
   [61 62 63 64 65 66 67 68 69]
   [71 72 73 74 75 76 77 78 79]
   [81 82 83 84 85 86 87 88 89]
   [91 92 93 94 95 96 97 98 99]])

(def digits (set "123456789"))

(def blank \0)

(defn read-grids []
  (->> (slurp "resources/p096_sudoku.txt")
       (clojure.string/split-lines)
       (partition-by (fn [line] (clojure.string/starts-with? line "Grid")))
       (partition 2)
       (map second)
       (map (fn [grid] (zipmap
                        (for [r-ix (range 9) c-ix (range 9)] [r-ix c-ix])
                        (apply concat grid))))
       ))

(defn print-grid [grid]
  (->> (range 9)
       (map (fn [r-ix]
              (println (apply str (for [c-ix (range 9)]
                                    (let [v (grid [r-ix c-ix])]
                                      (cond (set? v) \_
                                            (= \0 v) \_
                                            :else v)))))
              ))))

(def rows
  (->> (range 9)
       (map (fn [r-ix] (set (for [c-ix (range 9)] [r-ix c-ix]))))
       (vec)))

(def cols
  (->> (range 9)
       (map (fn [c-ix] (set (for [r-ix (range 9)] [r-ix c-ix]))))
       (vec)))

(def boxs
  (->> (for [top (range 0 9 3), left (range 0 9 3)] [top left])
       (map (fn [[top left]]
              (->> (for [r-ix (range top (+ 3 top))
                         c-ix (range left (+ 3 left))]
                     [r-ix c-ix])
                   (set)
                   )))
       (vec)))

(def conflicts
  (->> (for [r-ix (range 9) c-ix (range 9)] [r-ix c-ix])
       (map (fn [sqr]
              (->> (concat rows cols boxs)
                   (reduce (fn [conflict unit]
                             (if (unit sqr)
                               (union conflict unit)
                               conflict))
                           #{})
                   ((fn [conflict] [sqr (disj conflict sqr)]))
                   )))
       (apply concat)
       (apply hash-map)
       ))

(defn make-pgrid [grid]
  (reduce (fn [pgrid [sq v]]
            (assoc pgrid sq (if (= blank v) digits v)))
          {}
          grid
          ))

(defn pgrid-completed? [pgrid]
  (not (some set? (vals pgrid))))

(defn pgrid-resolved? [pgrid]
  (->> (concat rows cols boxs)
       (map (fn [unit] (set (map pgrid unit))))
       (remove (fn [vs] (== 9 (count vs))))
       (seq)
       (not)
       ))


(defn step-reduce-pgrid [pgrid]
  (try
    (reduce (fn [pgrid' [sq v]]
              (assoc pgrid' sq
                     (if (set? v)
                       (->> (conflicts sq)
                            (map pgrid)
                            (remove set?)
                            (set)
                            (difference v)
                            ((fn [v'] (case (count v')
                                        0 (throw RuntimeException)
                                        1 (first v')
                                        v' )))
                            )
                       v))
              )
            {}
            pgrid
            )
    (catch RuntimeException e nil)))

(defn reduce-pgrid [pgrid]
  (let [pgrid' (step-reduce-pgrid pgrid)]
    (if (= pgrid pgrid')
      pgrid
      (recur pgrid'))))

(defn iter-possibilities [pgrid]
  (->> (filter (fn [[sq v]] (set? v)) pgrid)
       (sort-by (fn [[_ v]] (count v)))
       (first)
       ((fn [[sq vs]] (map (fn [v] [sq v]) vs)))
       ))

(defn search-solution [pgrid]
  (let [r-pgrid (reduce-pgrid pgrid)]
    (when r-pgrid
      (if (pgrid-completed? r-pgrid)
        (if (pgrid-resolved? r-pgrid)
          r-pgrid
          nil)
        (->> (iter-possibilities r-pgrid)
             (map (fn [[sq v]] (assoc r-pgrid sq v)))
             (map search-solution)
             (some identity))
        ))
    ))

(defn solution []
  (->> (read-grids)
       (map make-pgrid)
       (map search-solution)
       (map (fn [grid] (+ (* 100 (- (int (grid [0 0])) 48))
                          (* 10 (- (int (grid [0 1])) 48))
                          (* 1(- (int (grid [0 2])) 48))
                          )))
       (reduce +)
       ))
