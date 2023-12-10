(require '[clojure.string :as str])

(def input-raw (slurp (-> *file* (str/split #"/") butlast (#(str/join "/" %)) (str "/input.txt"))))

#_(def input-raw "
0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45
")

(def input
  (->> input-raw
       str/trim
       (str/split-lines)
       (map (fn [line] (map parse-long (str/split line #" "))))))

(defn right-side [nums]
  (loop [arrays nums
         result []]
    (if (every? zero? arrays)
      (last (reductions + (map last result)))
      (recur (->> arrays (partition 2 1) (map #(- (apply - %))))
             (conj result arrays)))))

(defn left-side [nums]
  (loop [arrays nums
         result []]
    (if (every? zero? arrays)
      (last (reductions #(- %2 %1) (reverse (map first result))))
      (recur (->> arrays (partition 2 1) (map #(- (apply - %))))
             (conj result arrays)))))

(defn part1 [input]
  (->> input
       (map right-side)
       (reduce +)))

(defn part2 [input]
  (->> input
       (map left-side)
       (reduce +)))

(println "part1:" (part1 input))
(println "part2:" (part2 input))
