(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc base exp]
                 (if (zero? exp)
                   acc
                   (recur (* acc base) base (dec exp))))]
    (helper 1 base exp)))

(defn last-element [a-seq]
  (let [helper (fn [cur b-seq]
                 (if (empty? b-seq)
                   cur
                   (recur (first b-seq) (rest b-seq))))]
    (helper (first a-seq) (rest a-seq))))

(defn seq= [seq1 seq2]
  (loop [seq-a seq1
         seq-b seq2]
    (cond
     (and (empty? seq-a) (empty? seq-b))
      true
     (or (empty? seq-a) (empty? seq-b))
      false
     (= (compare (first seq-a) (first seq-b)) 0)
      (recur (rest seq-a) (rest seq-b))
     :else
      false)))

(defn find-first-index [pred a-seq]
  (loop [idx 0
         p pred
         seq-a a-seq]
    (cond
     (empty? seq-a)
      nil
     (p (first seq-a))
      idx
     :else
      (recur (inc idx) p (rest seq-a)))))

(defn avg [a-seq]
  (loop [elements 0
         sum 0
         seq-a a-seq]
    (if (empty? seq-a)
      (/ sum elements)
      (recur (inc elements) (+ sum (first seq-a)) (rest seq-a)))))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
  (disj a-set elem)
  (conj a-set elem)))

(defn parity [a-seq]
  (loop [seq-b #{}
         seq-a a-seq]
    (if (empty? seq-a)
      seq-b
      (recur (toggle seq-b (first seq-a)) (rest seq-a)))))

(defn fast-fibo [n]
  (loop [i 0
         j 1
         n n]
     (if (== n 0)
       i
       (recur j (+ i j) (dec n)))))

(defn cut-at-repetition [a-seq]
  (loop [seq-b []
         seq-a a-seq]
    (cond
     (empty? seq-a)
      seq-b
     (contains? (set seq-b) (first seq-a))
      seq-b
     :else
      (recur (conj seq-b (first seq-a)) (rest seq-a)))))

