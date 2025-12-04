(ns day-4.main
  (:require [clojure.string :as str]))

(defn parse [content]
  (->> (str/split-lines content)
      (mapv #(str/split % #""))))

(defn transform [matrix]
  (->> matrix
      (mapv (fn [row]
             (mapv #(if (= "@" %) 1 0)
                   row)))))

(defn adjacent-indices
  [x y width height]
  (for [dx [-1 0 1]
        dy [-1 0 1]
        :when (not (and (= dx 0) (= dy 0)))       ;; skip the center cell itself
        :let [nx (+ x dx) ny (+ y dy)]
        :when (and (<= 0 nx (dec width))
                   (<= 0 ny (dec height)))]
    [nx ny]))

(defn adjacent-cells
  [matrix]
  (let [height (count matrix)
        width  (count (first matrix))]
    (mapv
      (fn [y row]
        (mapv
          (fn [x v]
            (when (< 0 v)
              (let [neighbors (adjacent-indices x y width height)]
                (mapv (fn [[nx ny]] (get-in matrix [ny nx])) neighbors))))
          (range width) row))
      (range height) matrix)))

(defn summarizee
  [matrix]
  (->> matrix
       (mapv (fn [row]
               (mapv (fn [adjacents]
                       (when adjacents
                         (reduce + 0 adjacents))) row)))
       (reduce (fn [result row]
                 (->> row
                      (filter #(and (some? %) (> 4 %)))
                      (count)
                      (+ result)))
               0)))
(defn summarize [matrix]
  (->> matrix
       (mapcat identity)                       ; flatten to 1d seq of adjacents vecs
       (filter some?)                          ; drop nils, if any
       (map #(reduce + 0 %))                   ; sum each
       (filter #(< % 4))                       ; only sums < 4
       (count)))                               ; count them

(defn solve [file]
  (->> file
       (slurp)
       (parse)
       (transform)
       (adjacent-cells)
       (summarize)))

(println
 "\nTest\t  "
 (solve "./test_input.txt")
 "\nActual\t"
 (solve "./input.txt"))

