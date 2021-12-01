'{:dependencies [[org.clojure/clojure "1.10.1"]]}
(require '[clojure.string :as str])

(def input
  (->>
   (slurp "2017/day1/input.txt")
   (str/trim)
   (#(str/split % #""))
   (map #(Integer/parseInt %))))

; Example input
;; (def input (-> 91212129
;;                (str)
;;                (str/split #"")
;;                ((fn [s] (map #(Integer/parseInt %) s)))))

(defn match-at
  "Check if value matches list at index"
  [list index value]
  (let [length (count list)
        wrapped-index (mod (+ index length) length)]
    (= (nth list wrapped-index) value)))

(defn part1 []
  (prn "day1 part1:"
       (->>
        (map-indexed
         (fn [idx val]
           (when (match-at input (inc idx) val) val)) input)
        (remove nil?)
        (reduce +))))

(defn part2 []
  (prn "day1 part2:"
       (->>
        (map-indexed
         (fn [idx val]
           (when (match-at input (+ idx (/ (count input) 2)) val) val)) input)
        (remove nil?)
        (reduce +))))

(part1)
(part2)
