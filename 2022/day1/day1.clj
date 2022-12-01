(require '[clojure.string :as str])

()

;; (def input (slurp "2022/day1/input.txt"))
(def input "
1000
2000
3000

4000

5000
6000

7000
8000
9000

10000
")
(def input2
  (->>
   input
   str/trim
   (str/split-lines)
   (map (fn [line] line))))

(defn part1 [input]
  (->> input
       ))

(defn part2 [input])

(println "day1 part1:" (part1 input))
(println "day1 part2:" (part2 input))
