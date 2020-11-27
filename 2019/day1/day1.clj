'{:dependencies [[org.clojure/clojure "1.10.1"]]}
(require '[clojure.string :as str])

(def input
  (->>
   (slurp "2019/day1/input.txt")
   (str/split-lines)
   (map #(Integer/parseInt (str/trim %)))))

(defn calculate-fuel [mass] (- (quot mass 3) 2))

(defn calculate-total-fuel [mass]
  (let [fuel (calculate-fuel mass)]
    (if (< fuel 0)
      0
      (+ fuel (calculate-total-fuel fuel)))))

(defn part1 []
  (println "day1 part1:" (reduce + (map calculate-fuel input))))

(defn part2 []
  (println "day1 part2:" (reduce + (map calculate-total-fuel input))))

(part1)
(part2)

(comment
  "
   
  (defn calculate-total-fuel [mass]
    (reduce + (rest (take-while #(> % 0) (iterate calculate-fuel mass)))))
   
   #(> % 0) == pos? ;; Checks positive number

 ")
