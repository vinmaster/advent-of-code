(require '[clojure.string :as str]
         '[clojure.set :as set])

(def input-raw (slurp (str "2022/" (-> *file* (str/split #"/") butlast last) "/input.txt")))
#_(def input-raw "
2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8
")

(def input
  (->> input-raw
       str/trim
       str/split-lines
       (map (fn [line]
              (map (fn [num-range]
                     (->> (str/split num-range #"-") (map read-string)))
                   (str/split line #","))))))

(defn range-inclusive [start end] (range start (inc end)))

(defn part1 [input]
  (->> input
       (map (fn [pairs] (map #(apply range-inclusive %) pairs)))
       (filter (fn [[s1 s2]] 
                 (= (count (set/intersection (set s1) (set s2)))
                    (if (< (count s1) (count s2))
                      (count s1)
                      (count s2)))))
       count))

(defn part2 [input]
  (->> input
       (map (fn [pairs] (map #(apply range-inclusive %) pairs)))
       (filter (fn [[s1 s2]] (not= (count (concat s1 s2)) (count (distinct (concat s1 s2))))))
       count))

(prn "part1:" (part1 input))
(prn "part2:" (part2 input))
