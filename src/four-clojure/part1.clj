(ns four-clojure.part1)

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))


; problem30
(defn delrep [x]
  (loop [res []
        in (seq x)]
    (if (empty? in)
      (seq res)
      (recur (if (= (first in) (last res))
               res
               (conj res (first in)))
             (next in)))))

(delrep [1 2 2 3 3 3 4])
(delrep "Leeerroyyy")

; problem31
(defn packrep [x]
  (loop [res []
         in x]
    (if (empty? in)
      (seq res)
      (recur (if (= (first in) (first (last res)))
               (conj (pop res) (conj (peek res) (first in)))
               (conj res (list (first in))))
             (next in)))))

(packrep [1 1 2 2 2 3 4 4 4])

; proble33
(defn replicate-a-seq [x n]
  (reduce
    concat
    (map
      (fn construct-seq [ele]
        (loop [idx 0
               res '()]
          (if (= idx n)
            res
            (recur (+ idx 1)
                   (conj res ele)))))
      x)))

(replicate-a-seq [1 2 3] 2)


; problem41
(defn drop-every-Nth-item [x n]
  (loop [res []
         in x
         idx 1]
    (if (empty? in)
      res
      (recur (if (not= 0 (mod idx n))
               (conj res (first in))
               res)
             (next in)
             (+ idx 1)))))

(drop-every-Nth-item [1 2 3 4 5] 2)

;problem45
(= (seq (list 1 4 7 10 13)) (take 5 (iterate #(+ 3 %) 1)))
