(require '[clojure.string :as str])

(def input-raw (slurp (-> *file* (str/split #"/") butlast (#(str/join "/" %)) (str "/input.txt"))))

#_(def input-raw "
Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11")

(def input
  (->> input-raw
       str/trim
       str/split-lines))

(defn includes? [list val] (contains? (set list) val))

(defn parse-line [line]
  (let [[winning mine] (->> line (re-seq #"Card.*:(.*)\|(.*)") first rest)]
    {:winning (mapv first (re-seq #"(\d+)" winning))
     :mine (mapv first (re-seq #"(\d+)" mine))}))

(defn get-matches [card]
  (count (filter #(includes? (:winning card) %) (:mine card))))

(defn get-points [matches]
  (if (zero? matches)
    0
    (int (Math/pow 2 (dec matches)))))

(defn merge-with-lists [f & lists]
  (->> (map (fn [list] (apply merge (map-indexed #(apply hash-map (vector %1 %2)) list))) lists)
       (apply merge-with f)
       seq
       (sort-by first)
       (map second)))

(defn part1 [input]
  (->> input
       (mapv (comp get-points get-matches parse-line))
       (reduce +)))

(defn part2 [input]
  (let [cards (repeat (count input) 1)
        matches (map (comp get-matches parse-line) input)]
    (->> (reduce (fn [[cards i] match]
                   (let [adding (concat (repeat (inc i) 0) (repeat match (nth cards i)))
                         new-cards (merge-with-lists + adding cards)]
                     [new-cards (inc i)])) [cards 0] matches)
         first
         (reduce +))))

(println "part1:" (part1 input))
(println "part2:" (part2 input))
