(ns project-euler.problem_0100)

;; If a box contains twenty-one coloured discs,
;; composed of fifteen blue discs and six red discs, and two discs were taken at random,
;; it can be seen that the probability of taking two blue discs, P(BB) = (15/21)×(14/20) = 1/2.

;; The next such arrangement,
;; for which there is exactly 50% chance of taking two blue discs at random,
;; is a box containing eighty-five blue discs and thirty-five red discs.

;; By finding the first arrangement to contain over 10^12 = 1,000,000,000,000 discs in total,
;; determine the number of blue discs that the box would contain.


(defn solution-from [x]
  (let [r (Math/sqrt 0.5)]
    (->> (iterate inc x)
         (map
          (fn [x]
            (let [blue-1 (bigint (* r x))
                  blue   (inc blue-1)]
              [(== (*' 2 blue blue-1) (*' x (dec x))) blue x]
              )))
         (filter first)
         (first)
         (rest)
         )))

(defn solution []
  (loop [tot 21, r 5]
    (let [[blue tot'] (solution-from (bigint (* tot r)))]
      (if (< tot' 1000000000000)
        (recur tot' (/ tot' tot))
        blue)
      )))

;;-> 756872327473
