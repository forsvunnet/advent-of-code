(ns day-2.main
  (:require [clojure.string :as str]))

(defn parse [content]
  (mapv (fn [s]
          (mapv parse-long
                (str/split (str/trim s) #"-")))
        (str/split content #",")))

(defn find-invalid-ids-in-range [args]
  (let [r (range (first args) (last args))]
    (filterv
     (fn [v]
       (let [s (str v)
             len (count s)
             half (/ len 2)]
         (if (= 0 (mod len 2))
           (= (subs s 0 half) (subs s half))
           false)))
     r)))


(->> (slurp "./test_input.txt")
     (parse)
     (mapv find-invalid-ids-in-range)
     (filterv seq)
     (flatten)
     (reduce +)
     (println))

(->> (slurp "./input.txt")
     (parse)
     (mapv find-invalid-ids-in-range)
     (filterv seq)
     (flatten)
     (reduce +)
     (println))
                   
