(require '[clojure.string :as str])

(def input
  (->>
   (slurp "2021/day1/input.txt")
   (str/split-lines)
   (map #(Integer/parseInt (str/trim %)))))

#_(def input '[199 200 208 210 200 207 240 269 260 263])

(defn part1 [xs]
  (->> xs
       (partition 2 1)
       (filter #(apply < %))
       count))

(defn part2 [xs]
  (->> xs
       (partition 3 1)
       (map #(apply + %))
       part1))

(println "day1 part1:" (part1 input))
(println "day1 part2:" (part2 input))
