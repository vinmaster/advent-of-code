(require '[clojure.string :as str])

(defn parse-int [str & {:keys [base default] :or {base 10 default nil}}]
  (try (Integer/parseInt str base)
       (catch Exception _ default)))

(def input
  (->>
   (slurp "2021/day3/input.txt")
   #_"00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010"
   (str/split-lines)
   (map #(str/split % #""))))

(defn part1 [input]
  (let [gamma (-> (for [i (range 0 (count (first input)))
                        :let [col (reduce (fn [xs x] (conj xs (nth x i))) '[] input)
                              freq (frequencies col)
                              zeroes (get freq "0")
                              ones (get freq "1")]]
                    (if (> zeroes ones) "0" "1"))
                  str/join
                  (parse-int :base 2))
        epsilon (-> (for [i (range 0 (count (first input)))
                          :let [col (reduce (fn [xs x] (conj xs (nth x i))) '[] input)
                                freq (frequencies col)
                                zeroes (get freq "0")
                                ones (get freq "1")]]
                      (if (< zeroes ones) "0" "1"))
                    str/join
                    (parse-int :base 2))]
    (* gamma epsilon)))

(defn part2 [input]
  (let [oxygen (-> (loop [index 0
                          result ""
                          input input]
                     (if (not= (count input) 1)
                       (let [col (reduce (fn [xs x] (conj xs (nth x index))) '[] input)
                             freq (frequencies col)
                             zeroes (get freq "0")
                             ones (get freq "1")
                             end (if (> zeroes ones) "0" "1")
                             result (str result end)
                             input (filter #(-> % str/join (str/starts-with? result)) input)]
                         (recur (inc index) result input))
                       (-> input first str/join)))
                   (parse-int :base 2))
        co2 (-> (loop [index 0
                       result ""
                       input input]
                  (if (not= (count input) 1)
                    (let [col (reduce (fn [xs x] (conj xs (nth x index))) '[] input)
                          freq (frequencies col)
                          zeroes (get freq "0")
                          ones (get freq "1")
                          end (if (<= zeroes ones) "0" "1")
                          result (str result end)
                          input (filter #(-> % str/join (str/starts-with? result)) input)]
                      (recur (inc index) result input))
                    (-> input first str/join)))
                (parse-int :base 2))]
    (* oxygen co2)))

(println "day3 part1:" (part1 input))
(println "day3 part2:" (part2 input))

;; (comment
;;   (defn transpose [rows]
;;     (apply mapv vector rows))
;;   (defn advent-1 [rows]
;;     (->> (for [t (transpose rows)
;;                :let [f (frequencies t)]]
;;            (map first (sort-by second f)))
;;          (transpose)
;;          (map #(apply str %))
;;          (map #(parse-int % :base 2))
;;          (apply *))))
