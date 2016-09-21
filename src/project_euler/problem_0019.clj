(ns project-euler.problem_0019)

;; You are given the following information, but you may prefer to do some research for yourself.

;; 1 Jan 1900 was a Monday.
;; Thirty days has September,
;; April, June and November.
;; All the rest have thirty-one,
;; Saving February alone,
;; Which has twenty-eight, rain or shine.
;; And on leap years, twenty-nine.
;; A leap year occurs on any year evenly divisible by 4,
;; but not on a century unless it is divisible by 400.
;; How many Sundays fell on the first of the month
;; during the twentieth century (1 Jan 1901 to 31 Dec 2000)?

;; (defn leap? [year]
;;   (and (== 0 (rem year 4))
;;        (or (not  (== 0 (rem year 100)))
;;            (== 0 (rem year 400))) ))

(defn leap? [year]
  (cond (== 0 (rem year 400)) true
        (== 0 (rem year 100)) false
        (== 0 (rem year 4))   true
        :else                 false
        ))

(defn days-in-month
  [year month]
  (if (== 2 month)
    (if (leap? year)
      29
      28)
    ([nil 31 nil 31 30 31 30 31 31 30 31 30 31] month)
    ))

(defn days-in-year
  [year]
  (if (leap? year)
    366
    365)
  )

(defn days-from-1900
  [year month day]
  (+ (->> (range 1900 year)
          (map days-in-year)
          (reduce + 0))
     (->> (range 1 month)
          (map (partial days-in-month year))
          (reduce + 0))
     day
   ))

(defn sunday?
  [year month day]
  (== 0 (mod (days-from-1900 year month day) 7))
  )

(defn solution []
  (->> (for [year (range 1901 2001)
             month (range 1 13)]
         [year month])
       (filter (fn [[year month]] (sunday? year month 1)))
       (count)
       ))

;;-> 171

