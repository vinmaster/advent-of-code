(require '[clojure.string :as str])

(def input-raw (slurp (-> *file* (str/split #"/") butlast (#(str/join "/" %)) (str "/input.txt"))))

#_(def input-raw "
seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4")

(def input
  (-> input-raw
      str/trim
      (str/split #"\n\n")))

(defn includes? [list val] (contains? (set list) val))

(defn parse-mapping-blocks [blocks]
  (map (fn [block]
         (let [lines (str/split block #"\n")
               [from to] (->> lines first (re-matches #"(\w+)-to-(\w+).*") rest)
               setup-mapping (fn [[dest src len]]
                               {:src src
                                :dest (dec (+ src len))
                                :delta (- dest src)})
               mappings (->> lines rest (map (fn [line]
                                               (->> (str/split line #" ")
                                                    (map parse-long)
                                                    (setup-mapping)))))]
           {:from from
            :to to
            :mappings mappings}))
       blocks))

(defn parse-input [blocks]
  (let [seed-blocks (first blocks)
        mapping-blocks (rest blocks)]
    {:seeds (map (comp parse-long first) (re-seq #"(\d+)" seed-blocks))
     :mappings-blocks (parse-mapping-blocks mapping-blocks)}))

(defn get-next-value [mappings value]
  (let [in-mapping? (fn [{src :src dest :dest}] (<= src value dest))
        mapping (->> mappings (filter in-mapping?) first)]
    (if (empty? mapping)
      value
      (+ value (:delta mapping)))))

(defn part1 [input]
  (let [{seeds-init :seeds mappings-blocks :mappings-blocks} (parse-input input)]
    (->> (loop [category "seed"
                seeds seeds-init]
           (let [mappings-block (first (filter #(= (:from %) category) mappings-blocks))
                 next-values (map (partial get-next-value (:mappings mappings-block)) seeds)]
             (if (= category "location")
               seeds
               (recur (:to mappings-block)
                      next-values))))
         (apply min))))

(defn part2 [input])

(println "part1:" (part1 input))
(println "part2:" (part2 input))
