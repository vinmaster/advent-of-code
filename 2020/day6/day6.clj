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

;; (ns advent.2020.day6
;;   "Advent of Code 2020, day 6: Custom Customs"
;;   (:require [advent.helpers :as h]))

;; (def puzzle-input (h/slurp-resource "2020/day6.txt" h/slurp-lines))

;; (defn group-items [items]
;;   (when (seq items)
;;     (let [[x xs] (split-with #(not= "" %) items)]
;;       (cons x (group-items (rest xs))))))

;; (defn puzzle1 [input]
;;   (->> (group-items input)
;;        (map (comp count #(reduce into #{} %)))
;;        (apply +)))

;; (defn puzzle2 [input]
;;   (->> (group-items input)
;;        (mapcat #(distinct (apply concat %)))
;;        (count)))

;; --------------------------------------------------------------------------------

;; (ns adventofcode.2020.day6
;;   (:require
;;    [clojure.set :as set]
;;    [clojure.string :as string]))

;; (def input
;;   (string/split (slurp "resources/2020/day6.txt") #"\R\R"))


;; ;; Part 1

;; (->> input
;;      (map (fn [s] (count (set (apply str (string/split s #"\r\n"))))))
;;      (apply +))

;; ;; => 6625


;; ;; Part 2

;; (->> input
;;      (map (fn [s] (count (apply set/intersection (map set (string/split s #"\r\n"))))))
;;      (apply +))

;; ;; => 3360

;; --------------------------------------------------------------------------------

;; (ns aoc.2020.d06
;;   (:require [aoc.file-util :as file-util]
;;             [clojure.set :refer [intersection]]
;;             [clojure.string :as str]))

;; (def input (map str/split-lines (file-util/read-chunks "2020/d06.txt")))

;; (defn part-1 [input]
;;   (reduce + (map (comp count distinct (partial apply str)) input)))

;; (defn part-2 [input]
;;   (reduce + (map (comp count (partial apply intersection) (partial map set)) input)))
