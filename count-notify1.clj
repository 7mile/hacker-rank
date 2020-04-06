(ns clojure_noob.sort
  (:gen-class))

(defn get-median [expenditure]
  (let [middle (int (/ (count expenditure) 2))]
    (if (odd? (count expenditure))
      (nth expenditure middle)
      (/ (+ (nth expenditure middle) (nth expenditure (- middle 1))) 2))))

(defn remove-vec-loc [expenditure loc]
  (into (subvec expenditure 0 loc) (subvec expenditure (inc loc))))

(defn re-sort
  [expenditure old new]
  (let [old-loc (java.util.Collections/binarySearch expenditure old)
        exp-without-old (remove-vec-loc expenditure old-loc)
        new-loc (java.util.Collections/binarySearch exp-without-old new)]
    (if (>= new-loc 0)
      (vec (concat (subvec exp-without-old 0 new-loc) [new] (subvec exp-without-old new-loc)))
      (if (= -1 new-loc)
        (into [new] exp-without-old)
        (into (into (subvec exp-without-old 0 (- (- new-loc) 1)) [new]) (subvec exp-without-old (- (- new-loc) 1)))))))

(defn activityNotifications [expenditure d]
  (let [total-num (count expenditure)]
    (if (<= total-num d)
      0
      (loop [index d
             ordered-vec (vec (sort (subvec expenditure 0 d)))
             result 0]
        (if (>= index total-num)
          result
          (if (<= (* 2 (get-median ordered-vec)) (nth expenditure index))
            (recur (inc index) (re-sort ordered-vec (nth expenditure (- index d)) (nth expenditure index)) (inc result))
            (recur (inc index) (re-sort ordered-vec (nth expenditure (- index d)) (nth expenditure index)) result)))))))
