(require '[clojure.string :as str])

(def input-raw (slurp (-> *file* (str/split #"/") butlast (#(str/join "/" %)) (str "/input.txt"))))

#_(def input-raw "
Time:      7  15   30
Distance:  9  40  200")

(def input
  (-> input-raw
      str/trim
      (str/split-lines)))

#_(defn get-matches [time distance]
    (->> (range time)
         (map inc)
         (map (fn [velocity-time-left] (* velocity-time-left (- time velocity-time-left))))
         (drop-while neg?)
         (take-while pos?)
      ;;  (filter #(> % distance))
         count))

;; Performance improvement from Daniel
(defn get-matches [d record]
  (->> (range d)
       (map (fn [x] (+ (* (- x) x) (* d x) (- record))))
       (drop-while neg?)
       (take-while pos?)
       count))

(defn part1 [input]
  (let [times (map parse-long (re-seq #"\d+" (first input)))
        distances (map parse-long (re-seq #"\d+" (second input)))
        pairs (partition 2 (interleave times distances))]
    (->> pairs
         (map #(apply get-matches %))
         (reduce *))))

(defn part2 [input]
  (let [time (first (map parse-long (re-seq #"\d+" (str/replace (first input) #"\s+" ""))))
        distance (first (map parse-long (re-seq #"\d+" (str/replace (second input) #"\s+" ""))))]
    (get-matches time distance)))

(println "part1:" (part1 input))
(println "part2:" (part2 input))
