'{:dependencies [[org.clojure/clojure "1.10.1"]]}
(require '[clojure.string :as str])

(def input
  (->>
   (slurp "2020/day3/input.txt")
   (str/split-lines)))

(defn runSlope
  "Run down the slope on the map and return number of trees hit"
  [dx dy map]
  (loop [x dx y dy trees 0]
    (if (>= y (count map))
      trees
      (if (= (-> map (nth y) (nth (mod x (count (nth map y))))) \#)
        (recur (+ x dx) (+ y dy) (+ trees 1))
        (recur (+ x dx) (+ y dy) trees)))))

(defn part1 [input]
  (prn "day3 part1:"
       (runSlope 3 1 input)))

(defn part2 [input]
  (prn "day3 part2:"
       (->> (for [slope [[1 1] [3 1] [5 1] [7 1] [1 2]]]
              (runSlope (first slope) (second slope) input))
            (reduce *))))

(part1 input)
(part2 input)
