'{:dependencies [[org.clojure/clojure "1.10.1"]]}
(require '[clojure.string :as str])

(def input
  (->>
   (slurp "2020/day3/input.txt")
   (str/split-lines)))

(defn runSlope
  "Run down the slope on the map and return number of trees hit"
  [dx dy map]
  (loop [x dx y dy trees 0]
    (if (>= y (count map))
      trees
      (if (= (-> map (nth y) (nth (mod x (count (nth map y))))) \#)
        (recur (+ x dx) (+ y dy) (+ trees 1))
        (recur (+ x dx) (+ y dy) trees)))))

(defn part1 [input]
  (prn "day3 part1:"
       (runSlope 3 1 input)))

(defn part2 [input]
  (prn "day3 part2:"
       (->> (for [slope [[1 1] [3 1] [5 1] [7 1] [1 2]]]
              (runSlope (first slope) (second slope) input))
            (reduce *))))

(part1 input)
(part2 input)

;; (ns advent-of-code.2020.day3
;;   (:require [clojure.string :as s])
;;   (:gen-class))

;; (def grid (s/split-lines (slurp "resources/2020/day3.in")))

;; (defn is-tree?  [x y]
;;   (let [l (count (first grid))
;;         c (get (get grid x) (mod y l))]
;;     (if (= c \#) 1 0)))

;; (defn traverse
;;   ([dx dy] (traverse 0 0 dx dy 0))
;;   ([x y dx dy trees]
;;    (if (>= x (count grid))
;;      trees
;;      (traverse (+ x dx) (+ y dy) dx dy (+ trees (is-tree? x y))))))

;; (defn solution []
;;   (let [slopes [[1 1] [1 3] [1 5] [1 7] [2 1]]]
;;     (println (traverse 1 3)) ;; part 1
;;     (println (reduce #(* %1 (apply traverse %2)) 1 slopes)))) ;; part 2

;; --------------------------------------------------------------------------------

;; (def input (->> (slurp (io/resource "2020/3/input"))
;;                 str/split-lines))

;; (defn toboggan [input [dy dx]]
;;   (->> (for [i (range 0 (count input) dx)
;;              :let [line (get input i)
;;                    j (/ (* i dy) dx)]]
;;          (nth (cycle line) j))
;;        (filter #(= \# %))
;;        count))

;; (def movement [[1 1] [3 1] [5 1] [7 1] [1 2]])

;; (defn solution-1 [input]
;;   (toboggan input [3 1]))

;; (defn solution-2 [input]
;;   (apply * (for [move movement]
;;              (toboggan input move))))
