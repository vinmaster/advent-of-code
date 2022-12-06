(require '[clojure.string :as str])

(def input-raw (slurp (str "2022/" (-> *file* (str/split #"/") butlast last) "/input.txt")))
#_(def input-raw "mjqjpqmgbljsphdztnvjfqwrcgsmlb")

(def input (->> input-raw str/trim))

(defn unique-with-num [num]
  (fn [[n s]] [n (= num (count (distinct (take num (drop n s)))))]))

(defn part1 [input]
  (->> input
       (#(map vector (range (count %)) (repeat %)))
       (map (unique-with-num 4))
       (filter second)
       ffirst
       (+ 4)))

(defn part2 [input]
  (->> input
       (#(map vector (range (count %)) (repeat %)))
       (map (unique-with-num 14))
       (filter second)
       ffirst
       (+ 14)))

(prn "part1:" (part1 input))
(prn "part2:" (part2 input))

(comment 
  ;; replaces (#(map vector (range (count %)) (repeat %)))
  (prn (map (comp count set) (partition 4 1 input)) 4))
