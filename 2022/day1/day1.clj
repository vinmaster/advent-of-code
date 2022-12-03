(require '[clojure.string :as str])

(def input-raw (slurp "2022/day1/input.txt"))
#_(def input-raw "
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

(def input
  (->> input-raw
       str/trim
       (#(str/split % #"\n\n"))
       (map (fn [line] (map read-string (str/split line #"\n"))))))

(defn part1 [input]
  (->> input
       (map #(reduce + %))
       (apply max)))

(defn part2 [input]
  (->> input
       (map #(reduce + %))
       (sort >)
       (take 3)
       (reduce +)))

(prn "part1:" (part1 input))
(prn "part2:" (part2 input))
