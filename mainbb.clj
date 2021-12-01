;; (prn *command-line-args*)

(defn get-program-path [args]
  (if (= (count args) 2)
    (let [[year day] (->> args (map #(Integer/parseInt %)))]
      (format "%d/day%d/day%d.clj" year day day))
    (throw (Exception. "Args must be year and day"))))

;; (prn (get-command *command-line-args*))

(-> (shell/sh "bb" (get-program-path *command-line-args*))
    :out
    print)
