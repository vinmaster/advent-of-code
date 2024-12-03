(require '[clojure.string :as str])

(def input-raw (slurp (-> *file* (str/split #"/") butlast (#(str/join "/" %)) (str "/input.txt"))))

#_(def input-raw "
7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9")

(def input
  (->> input-raw
       str/trim
       str/split-lines
       (map (fn [line] (->> line (re-seq #"\d+") (map parse-long))))))

(defn safe? [list]
  (let [deltas (->> (partition 2 1 list)
                    (map #(apply - %)))
        all-inc-or-dec? #(or (every? pos? %)
                             (every? neg? %))
        within-bounds? (fn [xs] (every? #(<= 1 (abs %) 3) xs))]
    (and (all-inc-or-dec? deltas)
         (within-bounds? deltas))))

(defn removed-one-possibilities [list]
  (let [size (count list)]
    (for [x (range size)]
      (concat (take x list) (drop (inc x) list)))))

(defn part1 [input]
  (->> input
       (filter safe?)
       count))

(defn part2 [input]
  (->> input
       (filter (fn [list] (some safe? (conj (removed-one-possibilities list) list))))
       count))

(prn "part1:" (part1 input))
(prn "part2:" (part2 input))
