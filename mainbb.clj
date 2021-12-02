(import 'java.time.format.DateTimeFormatter
        'java.time.LocalDateTime)

(def date (LocalDateTime/now))
;; (def formatter (DateTimeFormatter/ofPattern "yyyy-MM-dd HH:mm:ss"))
(def formatter (DateTimeFormatter/ofPattern "yyyy"))

;; (prn *command-line-args*)

(defn get-program-path [args]
  (case (count args)
    1 (let [args (conj args (.format date formatter))
            [year day] (->> args (map #(Integer/parseInt %)))]
        (format "%d/day%d/day%d.clj" year day day))
    2 (let [[year day] (->> args (map #(Integer/parseInt %)))]
        (format "%d/day%d/day%d.clj" year day day))
    (throw (Exception. "Args must be year and day"))))

;; (prn (get-program-path *command-line-args*))

(-> (shell/sh "bb" (get-program-path *command-line-args*))
    :out
    print)
