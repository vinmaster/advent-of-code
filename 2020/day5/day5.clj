'{:dependencies [[org.clojure/clojure "1.10.1"]]}
(require '[clojure.string :as str])

(def input
  (->>
   (slurp "2020/day5/input.txt")
   (str/split-lines)))

(defn includes? [list val] (contains? (set list) val))

(defn seat-id [string]
  (->>
   (seq string)
   (reduce (fn [[front back left right] c]
             (case c
               \B [(+ front (Math/ceil (/ (- back front) 2))) back left right]
               \F [front (- back (Math/ceil (/ (- back front) 2))) left right]
               \R [front back (+ left (Math/ceil (/ (- right left) 2))) right]
               \L [front back left (- right (Math/ceil (/ (- right left) 2)))])) [0 127 0 7])
   ((fn [[front _ left _]] (+ (* front 8) left)))
   (int)))

(defn part1 []
  (prn "day5 part1:"
       (->>
        input
        (map seat-id)
        (apply max))))

(defn part2 []
  (prn "day5 part2:"
       (->>
        input
        (#(let [seat-ids (map seat-id %)
                max-id (apply max seat-ids)]
            (for [i (range 1 max-id)
                  :when (and  (not (includes? seat-ids i))
                              (includes? seat-ids (inc i))
                              (includes? seat-ids (dec i)))] i)))
        (first))))

(part1)
(part2)

;; (ns aoc.2020.d05
;;   (:require [aoc.file-util :as file-util]
;;             [clojure.string :as str]))

;; (def input (file-util/read-lines "2020/d05.txt"))

;; (defn seat-id [seat-code]
;;   (-> seat-code
;;       (str/escape {\B 1, \F 0, \R 1, \L 0})
;;       (Integer/parseInt 2)))

;; (defn part-1 [input] (apply max (map seat-id input)))

;; (defn part-2 [input]
;;   (let [taken-seats (sort (map seat-id input))
;;         all-seats (range (first taken-seats) (inc (last taken-seats)))]
;;     (first (clojure.set/difference (set all-seats) (set taken-seats)))))
