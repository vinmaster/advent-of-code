(require '[clojure.string :as str]
         '[clojure.set :as set])

(def input-raw (slurp (str "2022/" (-> *file* (str/split #"/") butlast last) "/input.txt")))
#_(def input-raw "    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2
")

(def input
  (->> input-raw
       (#(str/split % #"\n\n"))
       ((fn [[stacksInput proceduresInput]]
          (let [stacks-lines (->> stacksInput
                                  str/split-lines
                                  butlast
                                  (map #(re-seq #"\s{3}\s?|[A-Z]\s?" %)))
                stacks (take (count (first stacks-lines)) (repeat []))]
            {:stacks (->> (reduce (fn [acc line]
                                    (map flatten (map vector acc (map str/trim line))))
                                  stacks
                                  stacks-lines)
                          (map #(remove str/blank? %))
                          (mapv vec))
             :procedures (->> proceduresInput
                              str/split-lines
                              (map #(re-seq #"move (\d+) from (\d+) to (\d+)" %))
                              (map (comp vec #(map read-string %) rest first)))})))))

(defn part1 [input]
  (->> (reduce (fn [stacks [move from to]]
                 (-> stacks
                     (update (dec to) (fn [s] (concat (reverse (take move (nth stacks (dec from)))) s)))
                     (update (dec from) (fn [s] (drop move s)))))
               (:stacks input)
               (:procedures input))
       (map first)
       (str/join)))

(defn part2 [input]
  (->> (reduce (fn [stacks [move from to]]
                 (-> stacks
                     (update (dec to) (fn [s] (concat (take move (nth stacks (dec from))) s)))
                     (update (dec from) (fn [s] (drop move s)))))
               (:stacks input)
               (:procedures input))
       (map first)
       (str/join)))

(prn "part1:" (part1 input))
(prn "part2:" (part2 input))
