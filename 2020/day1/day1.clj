'{:dependencies [[org.clojure/clojure "1.10.1"]]}
(require '[clojure.string :as str])

(def input
  (->>
   (slurp "2020/day1/input.txt")
   (str/split-lines)
   (map #(Integer/parseInt (str/trim %)))))

(defn part1 []
  (prn "day1 part1:" (->> (for [num1 input
                                num2 input
                                :when (= (+ num1 num2) 2020)]
                            [num1 num2])
                          (map sort)
                          (distinct)
                          (map #(reduce * %))
                          (first))))

(defn part2 []
  (prn "day1 part2:" (->> (for [num1 input
                                num2 input
                                num3 input
                                :when (= (+ num1 num2 num3) 2020)]
                            [num1 num2 num3])
                          (map sort)
                          (distinct)
                          (map #(reduce * %))
                          (first))))

(part1)
(part2)
