(require '[clojure.string :as str])

(def input-raw (slurp (str "2022/" (-> *file* (str/split #"/") butlast last) "/input.txt")))
#_(def input-raw "30373
25512
65332
33549
35390")

(def input
  (->> input-raw
       str/trim
       str/split-lines
       (map #(map read-string (str/split % #"")))))

(defn find-index [pred coll]
  (first (keep-indexed #(when (pred %2) %1) coll)))

(def dirs [{:y -1 :x 0}
           {:y 1 :x 0}
           {:y 0 :x -1}
           {:y 0 :x 1}])

(defn is-inside [grid coord]
  (every? true? ((juxt #(>= (:x %) 0)
                       #(>= (:y %) 0)
                       #(< (:y %) (count grid))
                       #(< (:x %) (count grid))) coord)))

(defn collect-dirs [grid coord]
  (map (fn [dir]
         (->> coord
              (iterate (partial merge-with + dir))
              (drop 1)
              (take-while (partial is-inside grid))))
       dirs))

(defn decreasing-from [from]
  (fn [xs] (every? (partial > from) xs)))

(defn distance [from]
  (fn [xs]
    (let [found (find-index (partial <= from) xs)]
      (if (nil? found)
        (count xs)
        (inc found)))))

(defn get-value [grid coord]
  (nth (nth grid (:y coord)) (:x coord)))

(defn part1 [input]
  (->> input
       (map-indexed
        (fn [y row]
          (map-indexed
           (fn [x n] (if (some (decreasing-from n)
                               (map (fn [dirPath]
                                      (map (partial get-value input) dirPath))
                                    (collect-dirs input {:x x :y y})))
                       1
                       0))
           row)))
       flatten
       (apply +)))

(defn part2 [input]
  (->> input
       (map-indexed
        (fn [y row]
          (map-indexed
           (fn [x n] (->> (map (distance n) (map (fn [dirPath] (map (partial get-value input) dirPath))
                                                 (collect-dirs input {:x x :y y})))
                          (apply *)))
           row)))
       flatten
       (apply max)))

(prn "part1:" (part1 input))
(prn "part2:" (part2 input))
