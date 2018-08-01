(ns four-clojure.part1)

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))


; problem26
(defn fibonacci-sequaence [n]
  (loop [res [1]
         x0 0
         x1 1
         cnt 1]
    (if (= cnt n)
      (seq res)
      (recur (conj res (+ x0 x1))
             x1
             (+ x0 x1)
             (+ cnt 1)))))

(fibonacci-sequaence 8)

; problem 29
(def chac-vec
  (set (map char
       (range 65 91))))
chac-vec

(defn get-the-caps [str-arg]
  (def chac-vec
    (set (map char
         (range 65 91))))
  (loop [res []
         s (seq str-arg)]
    (if (empty? s)
      (apply str res)
      (recur (if (contains? chac-vec (first s))
               (conj res (first s))
               res)
             (next s)))))

(get-the-caps "HeLlO, WoRlD!")


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

; problem42
(defn factorial-fun [x]
  (loop [res 1
         cur-x x]
    (if (= cur-x 1)
      res
      (recur (* res cur-x)
             (- cur-x 1)))))

(factorial-fun 3)

; problem45
(= (seq (list 1 4 7 10 13)) (take 5 (iterate #(+ 3 %) 1)))


; problem48
6
(when true 6)

; problem52
(= [2 4] (let [[a b c d e f g] (range)] [c e]))
