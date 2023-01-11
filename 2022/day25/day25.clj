(require '[clojure.string :as str])

(def input-raw (slurp (str "2022/" (-> *file* (str/split #"/") butlast last) "/input.txt")))
#_(def input-raw "1=-0-2
12111
2=0=
21
2=01
111
20012
112
1=-1=
1-12
12
1=
122")

(def input
  (->> input-raw
       str/trim
       str/split-lines))

(def values
  {"=" -2 "-" -1 "0" 0 "1" 1 "2" 2})

(def powers-of-5 (iterate (partial * 5) 1))

(defn zip [list1 list2] (map vector list1 list2))
(defn zip' [list1 list2] (partition 2 (interleave list1 list2)))

(defn snafu->decimal [s]
  (->> (str/split s #"")
       reverse
       (zip powers-of-5)
       (map (fn [[v c]] (* v (get values c))))
       reverse
       (apply +)))

(defn decimal->snafu [n]
  (loop [n n
         s ""]
    (let [reminder (mod n 5)
          n (quot n 5)
          s (str (get ["0" "1" "2" "=" "-"] reminder) s)
          n (if (> reminder 2) (inc n) n)]
      (if (zero? n)
        s
        (recur n s)))))

(defn part1 [input]
  (->> input
       (map snafu->decimal)
       (apply +)
       decimal->snafu))

(prn "part1:" (part1 input))
