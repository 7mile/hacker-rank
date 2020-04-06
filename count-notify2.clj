(defn next-bigger [numbers n]
  (let [total (count numbers)]
    (loop [idx 0
           cur-bigger (Integer/MAX_VALUE)
           bigger-cnt 0]
      (if (= total idx)
        (if (> bigger-cnt (/ total 2))
          cur-bigger
          n)
        (if (<= (numbers idx) n)
          (recur (inc idx) cur-bigger bigger-cnt)
          (if (< (numbers idx) cur-bigger)
            (recur (inc idx) (numbers idx) (inc bigger-cnt))
            (recur (inc idx) cur-bigger (inc bigger-cnt))))))))


(defn next-smaller [numbers n]
  (let [total (count numbers)]
    (loop [idx 0
           cur-smaller (Integer/MIN_VALUE)
           smaller-cnt 0]
      (if (= total idx)
        (if (> smaller-cnt (/ total 2))
          cur-smaller
          n)
        (if (>= (numbers idx) n)
          (recur (inc idx) cur-smaller smaller-cnt)
          (if (> (numbers idx) cur-smaller)
            (recur (inc idx) (numbers idx) (inc smaller-cnt))
            (recur (inc idx) cur-smaller (inc smaller-cnt))))))))

(defn get-median-odd [cur-vec m old]
  (let [new (last cur-vec)]
    (if (= nil m)
      (nth (sort cur-vec) (/ (count cur-vec) 2))
      (cond
        (and (> m new) (> m old)) m
        (and (< m new) (< m old)) m
        (= new m) m
        (> new m) (next-bigger cur-vec m)
        (< new m) (next-smaller cur-vec m)))))

(defn avg-2 [i j]
  (/ (+ i j) 2))

(defn resort-get-ml-mh [vec-even]
  (let [num (count vec-even)
        v (sort vec-even)
        h (/ num 2)
        l (dec h)]
    [(nth v l) (nth v h)]))

(defn get-median-even [cur-vec ml mh]
  (let [new (last cur-vec)
        old (first cur-vec)]
    (if (= nil ml)
      (let [v (sort cur-vec)
            cnt (count cur-vec)
            h (/ cnt 2)
            l (dec h)]
        [(nth v l) (nth v h)])
      (cond
        (= new old) [ml mh]
        (and (> mh new) (> mh old)) [ml mh]
        (and (< ml new) (< ml old)) [ml mh]
        (and (> new mh) (< old ml)) [mh (next-bigger cur-vec mh)]
        (and (< new ml) (> old mh)) [ml (next-smaller cur-vec ml)]
        (and (< new mh) (> new ml) (<= old ml)) [new mh]
        (and (< new mh) (> new ml) (>= old mh)) [new ml]
        :else (resort-get-ml-mh cur-vec)))))

(defn notif-even [expenditure d]
  (let [total-num (count expenditure)]
    (loop [index d
           cur-vec (subvec expenditure 0 d)
           [ml mh] (get-median-even cur-vec nil nil)
           result 0]
      (if (>= index total-num)
        result
        (let [crt (nth expenditure index)
              nxt-vec (subvec expenditure (- index d -1) (inc index))
              [new-ml new-mh] (get-median-even nxt-vec ml mh)]
          (if (<= (+ ml mh) crt)
            (recur (inc index) nxt-vec [new-ml new-mh] (inc result))
            (recur (inc index) nxt-vec [new-ml new-mh] result)))))))

(defn notif-odd [expenditure d]
  (let [total-num (count expenditure)]
    (loop [index d
           cur-vec (subvec expenditure 0 d)
           m (get-median-odd cur-vec nil nil)
           result 0]
      (if (>= index total-num)
        result
        (let [nxt-vec (subvec expenditure (- index d -1) (inc index))
              new-m (get-median-odd nxt-vec m (first cur-vec))]
          (if (<= (* 2 m) (nth expenditure index))
            (recur (inc index) nxt-vec new-m (inc result))
            (recur (inc index) nxt-vec new-m result)))))))

(defn activityNotifications [expenditure d]
  (let [total-num (count expenditure)]
    (if (<= total-num d)
      0
      (if (odd? d)
        (notif-odd expenditure d)
        (notif-even expenditure d)))))
