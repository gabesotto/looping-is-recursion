(ns looping-is-recursion)


(defn power [base exp]
  (let [helper (fn [acc n k]
                 (if (zero? k)
                   acc
                   (recur (* acc n) n (dec k))))]
    (helper 1 base exp)))



(defn last-element [a-seq]
  (cond
   (empty? a-seq)         nil
   (empty? (rest a-seq))  (first a-seq)
   :else (recur (rest a-seq))))

(defn seq= [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq))  true
   (not= (count a-seq) (count b-seq))   false
   (= (first a-seq) (first b-seq))      (recur (rest a-seq) (rest b-seq))
   :else                                false))

(defn find-first-index [pred a-seq]
  (loop [index 0
         temp a-seq]
    (cond
      (empty? temp)       nil
      (pred (first temp)) index
      :else               (recur (inc index) (rest temp)))))

(defn avg [a-seq]
  (loop [quan 0
         sum  0
         seq  a-seq]
    (if (empty? a-seq) nil
      (if (empty? seq)
        (/ sum quan)
        (recur (inc quan) (+ sum (first seq)) (rest seq))))))


(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (loop [parity-set  (set ())
         seq         a-seq]
    (if (empty? seq)
      parity-set
      (recur (toggle parity-set (first seq)) (rest seq)))))

(defn fib [n]
  (cond
   (== n 0)  0
   (== n 1)  1
   :else     (+ (fib (- n 1)) (fib (- n 2)))))

(defn fast-fibo [n]
  (loop [step  0
         fib1 0
         fib2 1]

    (if (== step n)
      fib1
      (recur (inc step) (+ fib1 fib2) fib1))))

(defn cut-at-repetition [a-seq]
  (loop [z-seq      a-seq
         acc-seq    []]

    (cond
      (empty? z-seq)                           acc-seq
      (contains? (set acc-seq) (first z-seq))  acc-seq
      :else                                    (recur (rest z-seq) (conj acc-seq (first z-seq))))))



(cut-at-repetition ())
