(ns day-05
  (:require [clojure.string :as str]))

(defn parse-ranges [content]
  (->> (str/split content #"\n")
       (mapv (fn [line] (mapv parse-long (str/split line #"-"))))))

(defn parse-ids [content]
  (->> (str/split content #"\n")
       (mapv parse-long)))


(defn parse [content]
  (->> (str/split content #"\n\n")
       (mapv str/trim)
       ((fn [v] {:ranges (parse-ranges (get v 0))
                 :ids (parse-ids (get v 1))}))))

(defn is-id-in-range? [id range]
  (let [[start end] range]
    (<= start id end)))

(defn is-id-in-ranges? [id ranges]
  (some (fn [range] (is-id-in-range? id range)) ranges))

(defn count-ids-in-ranges [ids ranges]
  (count (filter (fn [id] (is-id-in-ranges? id ranges)) ids)))

(defn solve-part-1 [file]
  (->> (slurp file)
       (parse)
       ((fn [{:keys [ranges ids]}] (count-ids-in-ranges ids ranges)))))

(println "🧪 Part 1 (Test):" (solve-part-1 "day_05_test.txt"))
(println "🧪 Part 1 (Real):" (solve-part-1 "day_05_input.txt"))

(defn combine-overlapping-ranges [ranges]
  (->> ranges
       (sort-by first)
       (reduce (fn [acc range]
                 (if (empty? acc)
                   (conj acc range)
                   (let [[last-start last-end] (last acc)
                         [start end] range]
                     (if (<= start (inc last-end))
                       ;; Ranges overlap or are adjacent - merge them
                       (conj (vec (butlast acc)) [last-start (max last-end end)])
                       ;; Ranges don't overlap - add new range
                       (conj acc range)))))
               [])))


(defn calculate-range-size [range]
  (let [[start end] range]
    (inc (- end start))))

(defn solve-part-2 [file]
  (->> (slurp file)
       (parse)
       (#(combine-overlapping-ranges (:ranges %)))
       (reduce (fn [acc range]
                 (+ acc (calculate-range-size range))) 0)))

(println "🧪 Part 2 (Test):" (solve-part-2 "day_05_test.txt"))
(println "🧪 Part 2 (Real):" (solve-part-2 "day_05_input.txt"))