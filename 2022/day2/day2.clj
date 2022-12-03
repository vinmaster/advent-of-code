(require '[clojure.string :as str]
         '[clojure.set :as set])

(def input-raw (slurp "2022/day2/input.txt"))
#_(def input-raw "
A Y
B X
C Z
")

(def input
  (->> input-raw
       str/trim
       str/split-lines
       (map (fn [line] (str/split line #" ")))))

(defn index-of [x xs]
  (first (keep-indexed #(when (= %2 x) %1) xs)))

(def youList ["A" "B" "C"])
(def meList ["X" "Y" "Z"])
(def win-conditions {"Y" "A" "Z" "B" "X" "C"})
(def lose-conditions {"X" "B" "Y" "C" "Z" "A"})
(def tie-conditions {"X" "A" "Y" "B" "Z" "C"})

(defn score-round [you me]
  (+ (cond
       (= (win-conditions me) you) 6
       (= (lose-conditions me) you) 0
       (= (tie-conditions me) you) 3)
     (inc (index-of me meList))))

(defn part1 [input]
  (->> input
       (map #(apply score-round %))
       (apply +)))

(defn part2 [input]
  (let [choose (fn [you strategy] 
                 (case strategy
                   "X" ((set/map-invert lose-conditions) you)
                   "Y" ((set/map-invert tie-conditions) you)
                   "Z" ((set/map-invert win-conditions) you)))
        round-result (fn [you strategy]
                       [you (choose you strategy)])]
    (->> input
         (map #(apply round-result %))
         (map #(apply score-round %))
         (apply +)
         )))

(prn "part1:" (part1 input))
(prn "part2:" (part2 input))
