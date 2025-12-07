(ns day-06
  (:require [clojure.string :as str]))

(defn parse [content]
  ;; Split into lines
  (->> (str/split content #"\n")
       ;; Split each line into words
       (mapv (fn [line] (str/split line #" ")))
       ;; now we have a vector of vectors
       ;; last line is a vector of operators
       ;; rest are vectors of numbers
       ;; split into a map of :numbers and :operators
       ((fn [lines]
          {:numbers (butlast lines)
           :operators (last lines)}))))


(defn solve-part-1 [file]
  (->> (slurp file)
       (parse)))

(println "🧪 Part 1 (Test):" (solve-part-1 "day_06_test.txt"))
;(println "🧪 Part 1 (Real):" (solve-part-1 "day_06_input.txt"))

(defn solve-part-2 [file]
  (->> (slurp file)
       (parse)))

;(println "🧪 Part 2 (Test):" (solve-part-2 "day_05_test.txt"))
;(println "🧪 Part 2 (Real):" (solve-part-2 "day_05_input.txt"))