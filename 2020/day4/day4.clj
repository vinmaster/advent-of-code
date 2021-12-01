'{:dependencies [[org.clojure/clojure "1.10.1"]]}
(require '[clojure.string :as str])

(def input
  (->>
   (slurp "2020/day4/input.txt")
   (#(str/split % #"\n\n"))))

(defn parsePassports
  "Take input and parse into vector of passport map"
  [input]
  (->>
   input
   (map #(str/split %1 #"\n| "))
   (map
    (fn [entry] (reduce #(into %1 [(str/split %2 #":")]) {} entry)))))

(def passports (parsePassports input))

;; (defn includes? [list val] (some? (some (partial = val) list)))
;; (defn includes? [list val] (some #(= val %1) list))
(defn includes? [list val] (contains? (set list) val))

;; Use (Integer. val) to make sure value is int
(defn between? "Check if val is between low and high" [low high val] (<= low (Integer/parseInt (str val)) high))

(def req-fields ["byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"])

(def validator {"byr" (partial between? 1920 2002)
                "iyr" (partial between? 2010 2020)
                "eyr" (partial between? 2020 2030)
                "hgt" (fn [val] (let [[_ num unit] (re-matches #"^(\d+)(\w+)$" val)]
                                  (cond
                                    (= unit "cm") (between? 150 193 num)
                                    (= unit "in") (between? 59 76 num)
                                    :else false)))
                "hcl" (comp some? (partial re-matches #"^\#([0-9a-f]{6})$"))
                "ecl" (partial includes? ["amb" "blu" "brn" "gry" "grn" "hzl" "oth"])
                "pid" (comp some? (partial re-matches #"^(\d{9})$"))
                "cid" (fn [_] true)})

(defn has-req-fields? [passport] (every? (set (keys passport)) req-fields))

(defn is-valid? [passport] (every? #((get validator (first %1)) (second %1)) (seq passport)))

(defn part1 []
  (prn "day4 part1:"
       (->>
        passports
        (filter has-req-fields?)
        (count))))

(defn part2 []
  (prn "day4 part2:"
       (->>
        passports
        (filter #(and (has-req-fields? %) (is-valid? %)))
        (count))))

(part1)
(part2)

;; (defn parsePassports
;;   "Take input and parse into vector of passport map"
;;   [input]
;;   (->>
;;    input
;;    (map (fn [entry]
;;           (into {} (map (comp vec next) (re-seq #"(\w{3}):(\S+)" entry)))))
;;    vec))

;; --------------------------------------------------------------------------------

;; (def transpose
;;   (partial apply map vector))

;; (defn parse [passport]
;;   (let [[ks vs] (->> passport
;;                      (re-seq #"([^ \n]+):([^ \n]+)")
;;                      (map rest)
;;                      transpose)]
;;     (zipmap ks vs)))

;; (def passports
;;   (->> #"\n\n"
;;        (string/split (slurp "resources/2020/day4.txt"))
;;        (map parse)))

;; --------------------------------------------------------------------------------

;; (->>
;;      (string/split (get-res input-file) #"\n\n")

;;      ;; form:
;;      (map #(string/replace % #"\n" " "))
;;      (map #(string/split % #" "))

;;      ;; now, we have a collection of strings "key:val", so turn them into proper maps (still string-> string):
;;      (map
;;       (fn [coll]
;;         (reduce
;;          #(let [[key val] (string/split %2 #":")]
;;             (assoc %1 key val))
;;          {} coll))))

;; --------------------------------------------------------------------------------

;; (def mandatory-keys ["byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"])

;; (defn parse-input [str]
;;   (->> (str/split str #"\n\n")
;;        (map (fn [str]
;;               (->> (str/split str #"[\n| ]")
;;                    (map #(str/split % #":"))
;;                    (reduce #(assoc %1 (first %2) (second %2)) {}))))))

;; (defn valid-keys? [passport]
;;   (every? passport mandatory-keys))

;; (defn part-1 [file-name]
;;   (let [passports (parse-input (slurp file-name))]
;;     (count (filter valid-keys? passports))))

;; (defn parse-height [height-str suffix]
;;   (Integer/parseInt
;;    (.substring height-str 0 (str/index-of height-str suffix))))

;; (defn valid-height? [height-str]
;;   (cond
;;     (str/ends-with? height-str "cm") (<= 150 (parse-height height-str "cm") 193)
;;     (str/ends-with? height-str "in") (<= 59 (parse-height height-str "in") 76)
;;     :else false))

;; (defn valid-color? [color-str]
;;   (if-not (str/starts-with? color-str "#")
;;     false
;;     (let [color-values (.substring color-str 1)]
;;       (every? (fn [char-val]
;;                 (or (Character/isDigit char-val)
;;                     (<= (int \a) (int char-val) (int \f))))
;;               color-values))))

;; (def valid-eye-color-values #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"})

;; (defn valid-eye-color? [eye-color]
;;   (contains? valid-eye-color-values eye-color))

;; (defn valid-pid? [str-pid]
;;   (and (= 9 (count str-pid))
;;        (every? (fn [char]
;;                  (Character/isDigit char)) str-pid)))

;; (defn valid-value? [[key val]]
;;   (case key
;;     "byr" (<= 1920 (Integer/parseInt val) 2002)
;;     "iyr" (<= 2010 (Integer/parseInt val) 2020)
;;     "eyr" (<= 2020 (Integer/parseInt val) 2030)
;;     "hgt" (valid-height? val)
;;     "hcl" (valid-color? val)
;;     "ecl" (valid-eye-color? val)
;;     "pid" (valid-pid? val)
;;     "cid" true))

;; (defn valid-values? [passport]
;;   (every? valid-value? passport))

;; (defn part-2 [file-name]
;;   (let [passports (parse-input (slurp file-name))]
;;     (count (filter (fn [passport]
;;                      (and (valid-keys? passport)
;;                           (valid-values? passport)))
;;                    passports))))
