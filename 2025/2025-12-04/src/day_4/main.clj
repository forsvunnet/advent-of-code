(ns day-4.main
  (:require [clojure.string :as str]))

;; --- 🎄 Festive Formatting ---------------------------------------------------

(def style-reset  "\u001b[0m")
(def style-green  "\u001b[32m")
(def style-red    "\u001b[31m")
(def style-bold   "\u001b[1m")

(defn print-result [emoji label value]
  (println (str style-green emoji " " style-bold label style-reset 
                " " value)))

;; --- ⚙️ Data Processing ------------------------------------------------------

(defn parse [content]
  (->> (str/split-lines content)
       (mapv #(str/split % #""))))

(defn transform [matrix]
  (->> matrix
       (mapv (fn [row]
               (mapv #(if (= "@" %) 1 0) row)))))

;; --- 📐 Grid Logic -----------------------------------------------------------

(defn adjacent-indices [x y width height]
  (for [dx [-1 0 1]
        dy [-1 0 1]
        :when (not (and (= dx 0) (= dy 0)))
        :let [nx (+ x dx) ny (+ y dy)]
        :when (and (<= 0 nx (dec width))
                   (<= 0 ny (dec height)))]
    [nx ny]))

(defn adjacent-cells [matrix]
  (let [height (count matrix)
        width  (count (first matrix))]
    (mapv
      (fn [y row]
        (mapv
          (fn [x v]
            ;; Only look for neighbors if current cell is a paper roll (v > 0)
            (when (< 0 v) 
              (let [neighbors (adjacent-indices x y width height)]
                (mapv (fn [[nx ny]] (get-in matrix [ny nx])) neighbors))))
          (range width) row))
      (range height) matrix)))

;; --- 🚜 Removal Logic --------------------------------------------------------

(defn get-removals [matrix]
  (->> matrix
       (mapv (fn [row]
               (mapv (fn [adjacents]
                       ;; If we have neighbors (it's a roll) AND sum < 4
                       (if (and (some? adjacents)
                                (> 4 (reduce + 0 adjacents)))
                         1 0)) 
                     row)))))

(defn count-removals [matrix]
  (reduce + 0 (flatten matrix)))

(defn apply-removals [grid removals]
  (mapv (fn [row-grid row-rem]
          (mapv (fn [val-grid val-rem]
                  ;; If removal mask is 1, set to 0, otherwise keep value
                  (if (= 1 val-rem) 0 val-grid))
                row-grid row-rem))
        grid removals))

;; --- 🚀 Solvers --------------------------------------------------------------

(defn solve-part-1 [file]
  (->> (slurp file)
       (parse)
       (transform)
       (adjacent-cells)
       (get-removals)
       (count-removals)))

(defn solve-part-2 [file]
  (let [initial-grid (->> (slurp file)
                          (parse)
                          (transform))]
    (loop [current-grid  initial-grid
           total-removed 0]
      (let [matrix   (adjacent-cells current-grid)
            removals (get-removals matrix)
            n        (count-removals removals)]
        
        (if (zero? n)
          total-removed
          ;; Recur with updated grid AND updated total count
          (recur (apply-removals current-grid removals)
                 (+ total-removed n)))))))

;; --- 🏁 Main -----------------------------------------------------------------

(println (str style-red "\n🎅 --- North Pole Printing Department --- 🎅\n" style-reset))

(print-result "🧪" "Part 1 (Test):" (solve-part-1 "./test_input.txt"))
(print-result "📄" "Part 1 (Real):" (solve-part-1 "./input.txt"))

(println (str "\n" style-bold "--- Optimizing Forklift Paths... ---" style-reset))

(print-result "🧪" "Part 2 (Test):" (solve-part-2 "./test_input.txt"))
(print-result "🚜" "Part 2 (Real):" (solve-part-2 "./input.txt"))
(println "")
