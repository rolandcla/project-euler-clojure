(ns project-euler.core
  (:gen-class))

(defn run-problem
  [prob-n]
  (use (vec (list (symbol (str "project-euler.problem_" prob-n))
                  :only '(solution))))
  (eval '(solution))
  )

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  {:pre [(and args (== 1 (count args)))]}
  (println (time (run-problem (first args)))))
