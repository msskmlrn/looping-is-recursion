(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc n k]
                 (if (zero? k)
                   acc
                   (recur (* acc n) n (dec k))))]
    (helper 1 base exp)))

(defn last-element [a-seq]
  (let [helper (fn [acc a-seq]
                 (if (empty? a-seq)
                   acc
                   (recur (first a-seq) (rest a-seq))))]
    (helper nil a-seq)))

(defn seq= [seq1 seq2]
  (loop [seq1 seq1
         seq2 seq2]
    (cond
     (and (empty? seq1) (empty? seq2)) true
     (or (empty? seq1) (empty? seq2)) false
     (= (first seq1) (first seq2)) (recur (rest seq1) (rest seq2))
     :else false)))

(defn find-first-index [pred a-seq]
  (loop [i 0
         a-seq a-seq]
    (cond
     (empty? a-seq) nil
     (pred (first a-seq)) i
     :else (recur (inc i) (rest a-seq)))))

(defn avg [a-seq]
  (loop [sum 0
         members 0
         a-seq a-seq]
    (if (empty? a-seq) (if (== 0 members)
                       nil
                       (/ sum members))
      (recur (+ sum (first a-seq)) (inc members) (rest a-seq)))))

(defn parity [a-seq]
  (let [toggle (fn [a-set elem]
    (if (contains? a-set elem)
      (disj a-set elem)
      (conj a-set elem)))]
  (loop [a-seq a-seq
         a-set #{}]
    (if (empty? a-seq)
      a-set
      (recur (rest a-seq) (toggle a-set (first a-seq)))))))

(defn fast-fibo [n]
  (loop [a 0
         b 1
         i n]
    (if (zero? i)
      a
      (recur b (+ a b) (dec i)))))

(defn cut-at-repetition [a-seq]
  (loop [a-seq a-seq
         res []]
    (cond (empty? a-seq) res
          (some (fn [x] (= x (first a-seq))) res) res
          :else (recur (rest a-seq) (conj res (first a-seq))))))
