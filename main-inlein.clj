'{:dependencies [[org.clojure/clojure "1.10.1"]]}

(defn current-year []
  (.format (new java.text.SimpleDateFormat "yyyy") (java.util.Date.)))

(defn run [year day]
  (time (load-file (format "%s/day%s/day%s.clj" year day day)))
  ;; (when (resolve (symbol "part1")) (@(resolve (symbol "part1"))))
  ;; (when (resolve (symbol "part2")) (@(resolve (symbol "part2"))))
  )

(defn main []
  (let [args *command-line-args*]
    (cond
      (= (count args) 1) (run (current-year) (first args))
      (= (count args) 2) (run (first args) (second args)))))

(main)
