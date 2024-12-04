(require '[clojure.string :as str])

(def input-raw (slurp (-> *file* (str/split #"/") butlast (#(str/join "/" %)) (str "/input.txt"))))

#_(def input-raw "
3   4
4   3
2   5
1   3
3   9
3   3")

(def input
  (->> input-raw
       str/trim
       str/split-lines
       (map (fn [line] (->> line (#(str/split % #"   ")) (map parse-long))))))


(defn part1 [input]
  (->> input
       ((juxt #(map first %) #(map second %)))
       (map sort)
       (apply map vector)
       (map (comp abs #(apply - %)))
       (reduce +)))

(defn part2 [input]
  (let [list1 (map first input)
        list2 (map second input)]
    (->> list1
         (map (fn [n] (* n (count (filter #(= % n) list2)))))
         (reduce +))))

(prn "part1:" (part1 input))
(prn "part2:" (part2 input))
