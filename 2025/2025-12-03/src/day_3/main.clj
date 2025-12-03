(ns day-3.main
  (:require [clojure.string :as str]))

(defn parse [content]
  (->> (str/split-lines content)
      (mapv #(str/split % #""))))

(defn transform [banks]
  (mapv (fn [bank] (mapv #(parse-long %) bank))
        banks))

;; [8 1 8 1 8 1 **9** 1 1 1 1 **2** 1 1 1]
;;
(defn rx [result num]
  (if (or (empty? result)
          (> num (apply max result)))
    [num]
    (conj result num)))

(defn joltage [bank]
  (let [step1 (conj (reduce rx [] (pop bank))
                    (last bank))

        step2 (->> (subvec step1 1)
                  (reduce rx [] )
                  )]
       
    (-> (str (first step1)
             (first step2))
        parse-long)))

(->> (slurp "./test_input.txt")
     (parse)
     (transform)
     (mapv (fn ([bank]
                (joltage bank))))
     (reduce +)
     (println))

(->> (slurp "./input.txt")
     (parse)
     (transform)
     (mapv (fn ([bank]
                (joltage bank))))
     (reduce +)
     (println))
