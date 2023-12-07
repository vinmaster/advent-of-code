(require '[clojure.string :as str])

(def input-raw (slurp (-> *file* (str/split #"/") butlast (#(str/join "/" %)) (str "/input.txt"))))

#_(def input-raw "
32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483")

(def input
  (-> input-raw
      str/trim
      (str/split-lines)))

(defn includes? [list val] (contains? (set list) val))

(defn parse-hand [line]
  (let [[cards bid] (str/split line #" ")]
    {:cards cards :bid (parse-long bid)}))

(defn get-type [cards]
  (let [freq (frequencies cards)
        values (vals freq)
        key-count (count (keys freq))]
    (cond
      ;; 5 of a kind
      (includes? values 5) 7
      ;; 4 of a kind
      (includes? values 4) 6
      ;; Full house
      (and (includes? values 3) (= key-count 2)) 5
      ;; 3 of a kind
      (and (includes? values 3) (= key-count 3)) 4
      ;; 2 pair
      (and (includes? values 2) (= key-count 3)) 3
      ;; 1 pair
      (and (includes? values 2) (= key-count 4)) 2
      :else 1)))

(defn by-rank [hand1 hand2]
  (let [type-diff (compare (get-type (:cards hand1)) (get-type (:cards hand2)))]
    (if (zero? type-diff)
      (let [strength ["AKQJT98765432"]
            strength1 (->> (:cards hand1) vec (map str) (map (partial str/index-of strength)))
            strength2 (->> (:cards hand2) vec (map str) (map (partial str/index-of strength)))
            strength-diff (map #(apply - %1) (partition 2 (interleave strength2 strength1)))]
        (first (drop-while zero? strength-diff)))
      type-diff)))

(defn get-best-hand [hand]
  (let [freq (frequencies hand)
        j (get freq \J 0)]
    (if (= j 5)
      "AAAAA"
      (let [freq-pairs (->> freq (into [] cat) (partition 2) (filter #(not= (first %) \J)) (sort-by second) reverse)
            add-j (concat [[(ffirst freq-pairs) (+ j (second (first freq-pairs)))]] (rest freq-pairs))
            new-hand (apply str (mapcat (fn [[s n]] (repeat n s)) add-j))]
        new-hand))))

(defn by-rank-wild-j [hand1 hand2]
  (let [type-diff (compare (get-type (get-best-hand (:cards hand1))) (get-type (get-best-hand (:cards hand2))))]
    (if (zero? type-diff)
      (let [strength ["AKQT98765432J"]
            strength1 (->> (:cards hand1) vec (map str) (map (partial str/index-of strength)))
            strength2 (->> (:cards hand2) vec (map str) (map (partial str/index-of strength)))
            strength-diff (map #(apply - %1) (partition 2 (interleave strength2 strength1)))]
        (first (drop-while zero? strength-diff)))
      type-diff)))

(defn part1 [input]
  (->> input
       (map parse-hand)
       (sort by-rank)
       (map-indexed #(* (inc %1) (:bid %2)))
       (reduce +)))

(defn part2 [input]
  (->> input
       (map parse-hand)
       (sort by-rank-wild-j)
       (map-indexed #(* (inc %1) (:bid %2)))
       (reduce +)))

(println "part1:" (part1 input))
(println "part2:" (part2 input))
