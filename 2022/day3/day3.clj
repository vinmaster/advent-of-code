(require '[clojure.string :as str]
         '[clojure.set :as set])

(def input-raw (slurp "2022/day3/input.txt"))
#_(def input-raw "
vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw
")

(def input
  (->> input-raw
       str/trim
       str/split-lines))

(defn part1 [input]
  (let [char-seq #(seq (char-array %))
        split-half #(split-at (/ (count %) 2) (char-seq %))
        get-shared (fn [[comp1 comp2]] (str (first (set/intersection (set comp1) (set comp2)))))
        is-upper #(= % (str/upper-case %))
        priority (fn [c] (if (is-upper c) (- (.codePointAt c 0) 38) (- (.codePointAt c 0) 96)))
        sum #(apply + %)]
    (->> input
         (map (comp priority get-shared split-half))
         sum)))

(defn part2 [input]
  (let [get-shared (fn [[comp1 comp2 comp3]] (str (first (set/intersection (set comp1) (set comp2) (set comp3)))))
        is-upper #(= % (str/upper-case %))
        priority (fn [c] (if (is-upper c) (- (.codePointAt c 0) 38) (- (.codePointAt c 0) 96)))
        sum #(apply + %)]
    (->> input
         (partition 3)
         (map (comp priority get-shared))
         sum)))

(prn "part1:" (part1 input))
(prn "part2:" (part2 input))
