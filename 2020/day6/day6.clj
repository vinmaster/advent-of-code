'{:dependencies [[org.clojure/clojure "1.10.1"]]}
(require '[clojure.string :as str])
(require '[clojure.set])

(def input
  (->>
   (slurp "2020/day6/input.txt")
   (#(str/split % #"\n\n"))))

(defn part1 []
  (prn "day6 part1:"
       (->>
        input
        (map #(str/replace % #"\s" ""))
        (map (comp count set))
        (reduce +))))

(defn part2 []
  (prn "day6 part2:"
       (->>
        input
        (map #(str/split % #"\n"))
        (map (fn [x] (reduce clojure.set/intersection (map set x))))
        (map count)
        (reduce +))))

(part1)
(part2)
