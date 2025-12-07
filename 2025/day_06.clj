(ns day-06
  (:require [clojure.string :as str]))

(defn lines-to-numbers [lines]
  (mapv (fn [line] (mapv parse-long line)) lines))

(defn parse [content]
  ;; Split into lines
  (->> (str/split content #"\n")
       ;; Split each line into words
       (mapv (fn [line] (str/split (str/trim line) #"\s+")))
       ;; now we have a vector of vectors
       ;; last line is a vector of operators
       ;; rest are vectors of numbers
       ;; split into a map of :table and :operators
       ((fn [lines] {:table (lines-to-numbers (butlast lines))
                     :operators (last lines)}))))

(defn process [{:keys [table operators]}]
  (mapv (fn [[i op]]
          (let [numbers (mapv #(get % i) table)]
            (case op
              "+" (reduce + numbers)
              "*" (reduce * numbers)
              "-" (reduce - numbers)
              op)))
        (map-indexed vector operators)))

(defn solve-part-1 [file]
  (->> (slurp file)
       (parse)
       (process)
       (reduce +)))


(println "🧪 Part 1 (Test):" (solve-part-1 "day_06_test.txt"))
(println "🧪 Part 1 (Real):" (solve-part-1 "day_06_input.txt"))

(defn solve-part-2 [file]
  (->> (slurp file)
       (parse)))

;(println "🧪 Part 2 (Test):" (solve-part-2 "day_05_test.txt"))
;(println "🧪 Part 2 (Real):" (solve-part-2 "day_05_input.txt"))