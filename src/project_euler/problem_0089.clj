(ns project-euler.problem_0089)

;; For a number written in Roman numerals to be considered valid there are basic rules which must be followed.
;; Even though the rules allow some numbers to be expressed in more than one way
;; there is always a "best" way of writing a particular number.

;; For example, it would appear that there are at least six ways of writing the number sixteen:

;; IIIIIIIIIIIIIIII
;; VIIIIIIIIIII
;; VVIIIIII
;; XIIIIII
;; VVVI
;; XVI

;; However, according to the rules only XIIIIII and XVI are valid,
;; and the last example is considered to be the most efficient, as it uses the least number of numerals.

;; The 11K text file, roman.txt (right click and 'Save Link/Target As...'),
;; contains one thousand numbers written in valid, but not necessarily minimal, Roman numerals;
;; see About... Roman Numerals for the definitive rules for this problem.

;; Find the number of characters saved by writing each of these in their minimal form.

;; Note: You can assume that all the Roman numerals in the file contain no more than four consecutive identical units.

(def r-digits {\M 1000
               \D 500
               \C 100
               \L 50
               \X 10
               \V 5
               \I 1})

(defn from-roman [rom]
  (->> (range (count rom))
       (map (fn [i]
              (let [d (get r-digits (get rom i))
                    e (get r-digits (get rom (inc i)) 0)]
                (if (< d e) (- d) d)
                )))
       (reduce +)
       ))

(defn to-roman [x]
  (cond (zero? x) ""
        (>= x 1000) (str "M"  (to-roman (- x 1000)))
        (>= x 900)  (str "CM" (to-roman (- x 900)))
        (>= x 500)  (str "D"  (to-roman (- x 500)))
        (>= x 400)  (str "CD" (to-roman (- x 400)))
        (>= x 100)  (str "C"  (to-roman (- x 100)))
        (>= x 90)   (str "XC" (to-roman (- x 90)))
        (>= x 50)   (str "L"  (to-roman (- x 50)))
        (>= x 40)   (str "XL" (to-roman (- x 40)))
        (>= x 10)   (str "X"  (to-roman (- x 10)))
        (>= x 9)    (str "IX" (to-roman (- x 9)))
        (>= x 5)    (str "V"  (to-roman (- x 5)))
        (>= x 4)    (str "IV" (to-roman (- x 4)))
        (>= x 1)    (str "I"  (to-roman (- x 1)))
        ))

(defn solution []
  (->> (slurp "resources/p089_roman.txt")
       (clojure.string/split-lines)
       (map from-roman)
       (take 10)
       ))
