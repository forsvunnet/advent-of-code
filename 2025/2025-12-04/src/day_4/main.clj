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

(defn summarize [matrix]
  (->> matrix
       (mapcat identity)
       (filter some?)
       (map #(reduce + 0 %))
       (filter #(< % 4))
       (count)))

(defn get-removals [matrix]
  (->> matrix
       (mapv (fn [row]
               (mapv (fn [adjacents]
                       (if (and (some? adjacents)
                                (> 4 (reduce + 0 adjacents)))
                         1 0)) row)))))
(defn count-removals [matrix]
  (reduce + 0 (flatten matrix)))


(defn solve-part-1 [file]
  (->> file
       (slurp)
       (parse)
       (transform)
       (adjacent-cells)
       (get-removals)
       (count-removals)))

(defn apply-removals [grid removals]
  (mapv (fn [row-grid row-rem]
          (mapv (fn [val-grid val-rem]
                  ;; If the removal matrix says 1, set cell to 0. 
                  ;; Otherwise, keep the existing value.
                  (if (= 1 val-rem) 0 val-grid))
                row-grid row-rem))
        grid removals))

(defn solve-part-2 [file]
  (let [grid (->> file
                           (slurp)
                           (parse)
                           (transform))]
    ;; Loop tracks the CURRENT grid state
    (loop [current-grid grid
           total-removed 0]
      (let [matrix (adjacent-cells current-grid)
            removals   (get-removals matrix)
            n          (count-removals removals)]
        
        (if (zero? n)
          total-removed
          ;; RECUR: Pass the NEW grid and increment round
          (recur (apply-removals current-grid removals)
                 (+ total-removed n)))))))

(println ""
         (solve-part-1 "./test_input.txt") "\n"
         (solve-part-1 "./input.txt") "\n"
         (solve-part-2 "./test_input.txt") "\n"
         (solve-part-2 "./input.txt"))

