(defn init-arr [expenditure d]
  (let [arr (int-array 201)]
    (loop [idx 0]
      (if (= idx d)
        arr
        (let [v (nth expenditure idx)]
          (aset arr v (inc (nth arr v)))
          (recur (inc idx)))))))

(defn get-m-from-arr [m d]
  (let [^ints arr (m :arr)
        loc (+ 1 (int (/ d 2)))]
    (loop [idx 0
           total-cnt (aget arr idx)]
      (if (>= total-cnt loc)
        (assoc m :v idx)
        (let [nxt-idx (inc idx)
              nxt-cnt (aget arr nxt-idx)]
          (recur nxt-idx (+ total-cnt nxt-cnt)))))))

(defn next-num [^ints arr idx]
  (loop [i idx]
    (if (> (aget arr i) 0)
      i
      (recur (inc i)))))

(defn avg-2 [i j]
  (/ (+ i j) 2))

(defn median-of-even [m d]
  (let [^ints arr (m :arr)
        h-loc (inc (int (/ d 2)))
        l-loc (dec h-loc)]
    (loop [idx 0
           total-cnt (aget arr idx)]

      (if (>= total-cnt l-loc)
        (if (>= total-cnt h-loc)
          (assoc m :v idx)
          (assoc m :v (avg-2 idx (next-num arr (inc idx)))))

        (let [nxt-idx (inc idx)
              nxt-cnt (aget arr nxt-idx)]
          (recur nxt-idx (+ total-cnt nxt-cnt)))))))

(defn get-median [m new old cnt method]
  (let [^ints arr (m :arr)]
    (if-not (= new old)
      (do (aset arr old (dec (aget arr old)))
          (aset arr new (inc (aget arr new)))))
    (method m cnt)))

(defn notif [expenditure d]
  (let [f-vec (subvec expenditure 0 d)
        cnt (count expenditure)
        method (if (odd? d) get-m-from-arr median-of-even)]
    (loop [index d
           m (method {:arr (init-arr f-vec d)} d)
           result 0]
      (if (>= index cnt)
        result
        (let [crt (nth expenditure index)
              old (nth expenditure (- index d))]
          (if (<= (* 2 (m :v)) crt)
            (recur (inc index) (get-median m crt old d method) (inc result))
            (recur (inc index) (get-median m crt old d method) result)))))))

(defn activityNotifications [expenditure d]
  (let [total-num (count expenditure)]
    (if (<= total-num d)
      0
      (notif expenditure d))))

;; (def content (slurp "/Users/xiaofei.wang/input.txt"))
;; (def expenditure (vec (map #(Integer/parseInt %) (clojure.string/split content #" "))))
;; (time (activityNotifications expenditure 9999))  -> result of activityNotifications should be 629
;;  (cond
  ;;        (and (> m-v new) (> m-v old)) m
  ;;        (and (< m-v new) (< m-v old)) m
   ;;       (= new old m-v) m
    ;;      :else (get-m-from-arr m cnt))))))
;;   (assoc m :l (- total-cnt cnt) :h (- total-cnt 1) :c loc :v idx)
