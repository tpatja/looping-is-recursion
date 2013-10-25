(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc base exp]
                 (if (zero? exp) acc
                     (recur  (* acc base) base (- exp 1))))]
    (helper 1 base exp)))

(defn last-element [a-seq]
  (let [cnt (count a-seq)
        helper (fn [pos seq]
                 (if (== pos (- cnt 1)) (nth seq pos)
                     (recur (+ pos 1) seq)))]
    (if (empty? a-seq) nil
        (helper 0 a-seq))))

(defn seq= [seq1 seq2]
  (let [helper (fn [s1 s2]
                 (cond
                     (and (empty? s1) (empty? s2)) true
                     (or (empty? s1) (empty? s2)) false
                     :else (and (= (first s1) (first s2)) (recur (rest s1) (rest s2)))))]
    (helper seq1 seq2)))

(defn find-first-index [pred a-seq]
  (loop [idx 0]
    (cond (>= idx (count a-seq)) nil
          (true? (pred (nth a-seq idx))) idx
          :else (recur (inc idx)))))

(defn avg [a-seq]
  (let [cnt (count a-seq)]
    (loop [acc-sum 0 
           idx 0 
           seq a-seq]
      (cond (>= idx cnt) (/ acc-sum cnt)
            :else (recur (+ acc-sum (first seq)) (inc idx) (rest seq))) )))


(defn parity [a-seq]
  (let [toggle (fn [a-set elem]
                 (if (contains? a-set elem)
                   (disj a-set elem)
                   (conj a-set elem)))]
    (if (empty? a-seq) '()
      (loop [aset #{} 
             aseq a-seq]
        (if (empty? aseq) aset
            (recur (toggle aset (first aseq)) (rest aseq)))))))


(defn fast-fibo [n]
  (cond (zero? n) 0
        (= n 1) 1
        :else (loop [x 1 
                     acc 1
                     idx 2]
                (if (== idx n) acc
                    (recur acc (+ x acc) (inc idx))))))


(defn cut-at-repetition [a-seq]
  (empty? a-seq) nil
  (loop [idx 0 
           s []]
      (let [cur (nth a-seq idx)
            dupe (contains? (set s) cur)]
        (cond
         (and (not dupe) (>= idx (dec (count a-seq)))) (conj s cur)
         (contains? (set s) cur) s
         :else (recur (inc idx) (conj s cur))))))

