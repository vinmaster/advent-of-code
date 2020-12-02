'{:dependencies [[org.clojure/clojure "1.10.1"]]}
(require '[clojure.string :as str])

(def input
  (->>
   (slurp "2020/day1/input.txt")
   (str/split-lines)
   (map #(Integer/parseInt (str/trim %)))))

(defn part1 []
  (prn "day1 part1:" (->> (for [num1 input
                                num2 input
                                :when (= (+ num1 num2) 2020)]
                            [num1 num2])
                          (map sort)
                          (distinct)
                          (map #(reduce * %))
                          (first))))

(defn part2 []
  (prn "day1 part2:" (->> (for [num1 input
                                num2 input
                                num3 input
                                :when (= (+ num1 num2 num3) 2020)]
                            [num1 num2 num3])
                          (map sort)
                          (distinct)
                          (map #(reduce * %))
                          (first))))

(part1)
(part2)

;; (comment
;;   "
;; (ns advent-of-code.day01
;;   (:require [clojure.math.combinatorics :as comb]))

;; (defn- read-numbers [data]
;;   (->> data
;;        (re-seq #"-?\d+")
;;        (map #(Integer/parseInt %))))

;; (defn part-1
;;   "Day 01 Part 1"
;;   [input]
;;   (as-> input $
;;     (read-numbers $)
;;     (comb/combinations $ 2)
;;     (filter #(= 2020 (apply + %)) $)
;;     (apply * (first $))))

;; (defn part-2
;;   "Day 01 Part 2"
;;   [input]
;;   (as-> input $
;;     (read-numbers $)
;;     (comb/combinations $ 3)
;;     (filter #(= 2020 (apply + %)) $)
;;     (apply * (first $))))

;; --------------------------------------------------------------------------------

;; (require '[clojure.math.combinatorics :as comb])
;; (let [numbers (read-string (format "[%s]" (slurp "1.txt")))]
;;   (->> (comb/combinations numbers 3)
;;        (filter #(= 2020 (apply + %)))
;;        first
;;        (apply *)))

;; --------------------------------------------------------------------------------

;; (ns aoc.2020.d01
;;   (:require [aoc.file-util :as file-util]
;;             [clojure.math.combinatorics :as combo]))

;; (def input (sort (file-util/read-values "2020/d01.txt")))

;; (defn solve
;;   "Returns the product of the `n` numbers from set of `input` that sum to `target-sum`"
;;   [input target-sum n]
;;   (->> (combo/combinations input n)
;;        (some #(if (= target-sum (apply + %)) %))
;;        (reduce *)))

;; (defn part-1 [input] (solve input 2020 2))

;; (defn part-2 [input] (solve input 2020 3))
;;  ")
