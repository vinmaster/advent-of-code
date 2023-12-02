(require '[clojure.string :as str])

(def input-raw (slurp (-> *file* (str/split #"/") butlast (#(str/join "/" %)) (str "/input.txt"))))

#_(def input-raw "
Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green")

(def input
  (->> input-raw
       str/trim
       str/split-lines))

(defn tap->> [f x] (f x) x)

(defn game-set->map [s] (->> s (mapv (fn [[color n]] [color (read-string n)])) (mapv #(apply hash-map %)) (apply merge)))

(defn parse-game [game]
  (let [[id setsStr] (->> game (re-seq #"Game (\d+): (.*)") first rest)
        sets (-> setsStr (str/split #"; "))
        game-sets (->> sets (mapv (fn [s] (->> s (re-seq #"(\d+) (\w+)(\,\s)?") (mapv (#(comp rest butlast reverse))) game-set->map))))]
    {:id (read-string id) :sets game-sets}))

(def max-game-set {"red" 12 "green" 13 "blue" 14})

(defn part1 [input]
  (->> input
       (mapv parse-game)
       (filterv (fn [{game-sets :sets}] (->> game-sets (mapv (fn [s] (apply min (vals (merge-with - max-game-set s))))) (apply min) neg? not)))
       (mapv :id)
       (reduce +)))

(defn part2 [input]
  (->> input
       (mapv parse-game)
       (mapv (fn [{game-sets :sets}] (->> game-sets (reduce #(merge-with max %1 %2)) vals (reduce *))))
       (reduce +)))

(println "part1:" (part1 input))
(println "part2:" (part2 input))
