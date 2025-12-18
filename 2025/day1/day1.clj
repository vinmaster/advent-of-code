(require '[clojure.string :as str])

(def input-raw (slurp (-> *file* (str/split #"/") butlast (#(str/join "/" %)) (str "/input.txt"))))

#_(def input-raw "
L68
L30
R48
L5
R60
L55
L1
L99
R14
L82")

(def input
  (->> input-raw
       str/trim
       str/split-lines))

(defn rotate [start rotation]
  (let [dir (first rotation)
        step (Integer/parseInt (apply str (rest rotation)))]
    (case dir
      \L (mod (- start step) 100)
      \R (mod (+ start step) 100))))

(defn rotate-by-one [start rotation]
  (let [dir (first rotation)
        step (inc (Integer/parseInt (apply str (rest rotation))))]
    (case dir
      \L (take step (iterate (fn [x] (mod (dec x) 100)) start))
      \R (take step (iterate (fn [x] (mod (inc x) 100)) start)))))

(prn (rotate-by-one 50 "L68")
     (filter zero? (rotate-by-one 50 "L68")))

(defn part1 [input]
  (->> input
       (reductions rotate 50)
       (filter zero?)
       count))

(defn part2 [input]
  (->> input
       (reduce
        (fn [acc rotation]
          (conj acc (vec (rotate-by-one (last (last acc)) rotation))))
        [[50]])
       (map butlast)
       (map #(count (filter zero? %)))
       (reduce +)))

(prn "part1:" (part1 input))
(prn "part2:" (part2 input))

#_(comment

    (defn parse-input [fname]
      (for [line (-> fname slurp str/split-lines)
            :let [d (first line)
                  n (->> line rest (apply str) parse-long)]]
        [d n]))

    (defn next-number [[direction amt] current]
      (case direction
        \L (mod (+ (- current amt) 100) 100)
        \R (mod (+ current amt) 100)))

    (defn rotate-dial-part-1 [input]
      (loop [[instruction & rest] (parse-input input)
             position             50
             n-zeros              0]
        (if (seq instruction)
          (let [n-position (next-number instruction position)]
            (recur rest
                   n-position
                   (if (zero? n-position) (inc n-zeros) n-zeros)))
          n-zeros)))

    (defn rotate-dial-part-2 [input]
      (loop [[instruction & rest] (parse-input input)
             position             50
             n-zeros              0]
        (if (seq instruction)
          (let [n-position (next-number instruction position)]
            (recur rest
                   n-position
                   (let [[dir amt] instruction]
                     (->>
                      (filterv #(zero? (mod % 100))
                               (case dir
                                 \L (range (- position amt) position)
                                 \R (range (inc position) (inc (+ position amt)))))
                      count
                      (+ n-zeros)))))
          n-zeros)))

    (defn day01 []
      (let [input "inputs/day01.txt"]
        (println "Part 1:" (rotate-dial-part-1 input))
        (println "Part 2:" (rotate-dial-part-2 input)))))
