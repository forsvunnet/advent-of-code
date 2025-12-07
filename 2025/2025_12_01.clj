(ns day-1.main
  (:require [clojure.string :as str]))

(let [content (slurp "./input.txt")
      lines (str/split-lines content)
      nums (mapv (fn [line]
                    (let [dir (subs line 0 1)
                          num (Integer/parseInt (subs line 1))]
                      (if (= dir "L")
                        (- num)
                        num)))
                  lines)
      zeros  (reduce
                   (fn [acc num]
                     (conj acc
                           (mod (+ (last acc) num) 100)))
                   [50] nums)
      zero-count (reduce
                  (fn [acc num]
                    (+ acc
                       (if (= num 0)
                          1
                          0)))
                  0
                  zeros)]
  (println zero-count))
