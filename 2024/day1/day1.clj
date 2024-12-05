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

#_(comment
    (defn part-1 [lines]
      (->> lines
           (map #(mapv parse-long (str/split % #" +")))
           (apply map (comp sort vector)) ; transpose to get lists, then sort
           (apply map (comp abs -))       ; un-transpose and find the abs diff between each element
           (reduce +)))                   ; sum

    (defn part-2 [lines]
      (let [[a b] (->> lines
                       (map #(mapv parse-long (str/split % #" +")))
                       (apply map vector))]  ; get transposed lists
        (as-> b x
      ; count each element in b, storing counts in a map
          (reduce (fn [table n] (update table n #(inc (or % 0)))) {} x)
      ; multiply each element of a with its count in b
          (map #(* (x % 0) %) a)
      ; sum
          (reduce + x))))

; --------------------------------------------------------------------------------

    (require '[clojure.string :as str])

    (def pairs (->> (slurp "in.txt")
                    str/split-lines
                    (map #(str/split % #"   "))))

    (def lefts (sort (map #(Integer/parseInt (first %)) pairs)))
    (def rights (sort (map #(Integer/parseInt (second %)) pairs)))

    (println (reduce + (map #(abs (- %1 %2)) lefts rights)))
    (println (reduce + (map #(* % ((frequencies rights) % 0)) lefts))))
