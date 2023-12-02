(require '[clojure.string :as str])

(def input-raw (slurp (-> *file* (str/split #"/") butlast (#(str/join "/" %)) (str "/input.txt"))))

#_(def input-raw "
1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet")

#_(def input-raw "
two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen")

(def input
  (->> input-raw
       str/trim
       str/split-lines))

(def numbers [nil "one" "two" "three" "four" "five" "six" "seven" "eight" "nine"])

;; https://stackoverflow.com/questions/3262195/compact-clojure-code-for-regular-expression-matches-and-their-position-in-string
(defn re-pos [re s]
  (if (nil? re)
    {}
    (loop [m (re-matcher re s)
           res {}]
      (if (.find m)
        (recur m (assoc res (.start m) (.group m)))
        res))))

;; Returns sequence of vector pairs [index value]
(defn get-number-indexes [s]
  (mapv (fn [[i n]] [i (read-string n)])
        (vec (re-pos #"\d" s))))

(defn get-word-indexes [s]
  (mapv (fn [[i word]] [i (.indexOf numbers word)])
        (mapcat #(re-pos (re-pattern %) s) (remove nil? numbers))))

(defn get-first-last-as-int [indexes]
  (->> indexes
       (#((juxt first last) %))
       (map second)
       (apply str)
       read-string))

(defn part1 [input]
  (->> input
       (mapv (fn [line]
               (->> line
                    get-number-indexes
                    (sort-by first)
                    get-first-last-as-int)))
       (reduce +)))

(defn part2 [input]
  (->> input
       (mapv (fn [line]
               (->> line
                    get-number-indexes
                    (concat (get-word-indexes line))
                    (sort-by first)
                    get-first-last-as-int)))
       (reduce +)))

;; (prn "part1:" (part1 input))
;; (prn "part2:" (part2 input))
(println "part1:" (part1 input))
(println "part2:" (part2 input))
