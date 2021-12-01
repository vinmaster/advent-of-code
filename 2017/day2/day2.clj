'{:dependencies [[org.clojure/clojure "1.10.1"]]}
(require '[clojure.string :as str])

(defn parse-line [line] (map #(Integer/parseInt %) (str/split line #"\s")))

(def input
  (->>
   (slurp "2017/day2/input.txt")
   (str/trim)
   (str/split-lines)
   (map parse-line)))

(defn diff [list] (- (apply max list) (apply min list)))

(defn evenly-divides [list] (first (distinct (for [num1 list
                                                   num2 list
                                                   :let [smaller (min num1 num2)
                                                         bigger (max num1 num2)]
                                                   :when (not= num1 num2)
                                                   :when (zero? (mod bigger smaller))]
                                               (quot bigger smaller)))))

(defn part1 []
  (prn "day2 part1:" (reduce + (map diff input))))

(defn part2 []
  (prn "day2 part2:" (reduce + (map evenly-divides input))))

(part1)
(part2)
