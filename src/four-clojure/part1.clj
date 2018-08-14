(ns four-clojure.part1
  (require [clojure.set :as set]))

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

; problem43
(defn reverse-interleave [s n]
  (loop [res-map {}
        s-tmp (seq s)
        idx 0]
    (if (empty? s-tmp)
      (seq (map seq (vals res-map)))
      (recur (assoc res-map
               (mod idx n)
               (conj
                 (if (get res-map (mod idx n))
                   (get res-map (mod idx n))
                   [])
                 (first s-tmp)))
             (next s-tmp)
             (inc idx)))))

(reverse-interleave [1 2 3 4 5] 2)

(= (reverse-interleave [1 2 3 4 5 6] 2)
   '((1 3 5) (2 4 6)))
(= (reverse-interleave (range 9) 3)
   '((0 3 6) (1 4 7) (2 5 8)))
(= (reverse-interleave (range 10) 5)
   '((0 5) (1 6) (2 7) (3 8) (4 9)))

; problem44
(defn rotate-a-sequence [n in]
  (loop [res-vec (into [] in)
        n-tmp n]
    (cond
      (= 0 n-tmp) (seq res-vec)
      (> n-tmp 0) (recur (conj (into [] (next res-vec)) (first res-vec))
                         (dec n-tmp))
      (< n-tmp 0) (recur (into (vector (peek res-vec)) (pop res-vec))
                         (inc n-tmp)))))

(rotate-a-sequence -2 [1 2 3 4 5])

(= (rotate-a-sequence 2 [1 2 3 4 5])
   '(3 4 5 1 2))

(into [] '(1 2 3 4))
(seq [1 2 3])

; problem45
(= (seq (list 1 4 7 10 13)) (take 5 (iterate #(+ 3 %) 1)))

; problem46
(defn flipping-out [f]
  (fn [a b]
    (f b a)))

(= 3 ((flipping-out nth) 2 [1 2 3 4 5]))
(= true ((flipping-out >) 7 8))
(= 4 ((flipping-out quot) 2 8))
(= [1 2 3] ((flipping-out take) [1 2 3 4 5] 3))

; problem48
6
(when true 6)

; problem50
(defn split-by-type [input-s]
  (loop [res-map {}
         tmp-s input-s]
    (if (empty? tmp-s)
      (into #{} (vals res-map))
      (if (= nil (get res-map (class (first tmp-s))))
        (recur (assoc res-map
                 (class (first tmp-s))
                 (vector (first tmp-s)))
               (next tmp-s))
        (recur (assoc res-map
                 (class (first tmp-s))
                 (conj (get res-map (class (first tmp-s))) (first tmp-s)))
               (next tmp-s))))))

(split-by-type [:b :a 1 2])

(= (set (split-by-type [1 :a 2 :b 3 :c])) #{[1 2 3] [:a :b :c]})
(= (set (split-by-type [:a "foo" "bar" :b])) #{[:a :b] ["foo" "bar"]})
(= (set (split-by-type [[1 2] :a [3 4] 5 6 :b])) #{[[1 2] [3 4]] [:a :b] [5 6]})

; problem51
(= [1 2 [3 4 5] [1 2 3 4 5]] (let [[a b & c :as d] [1 2 3 4 5]] [a b c d]))


; problem52
(= [2 4] (let [[a b c d e f g] (range)] [c e]))


; problem66
(defn gcd [a b]
  (if (= b 0)
    a
    (gcd b (mod a b))))

(gcd 4 2)

; problem67
(defn get-sqrt [n]
  (loop [tmp-n 0]
    (if (> (* tmp-n tmp-n) n)
      (- tmp-n 1)
      (recur (+ tmp-n 1)))))

(defn prime? [n]
  (let [sqrt-num (get-sqrt n)]
    (loop [tmp-i 2]
      (if (or (> tmp-i sqrt-num) (= tmp-i n))
        true
        (if (= 0 (mod n tmp-i))
          false
          (recur (+ tmp-i 1)))))))

(defn prime-numbers [n]
  (into []
        (take n
          (filter #(and (prime? %) (not= % 1)) (iterate #(+ % 1) 1)))))

(prime-numbers 5)
(= (last (prime-numbers 100))
   541)


; problem83
(defn a-half-true [a & other]
  (if (and (not (reduce #(if (and (= true %1)
                                  (= true %2))
                           true
                           false)
                        (conj other a)))
           (reduce #(if (or (= true %1)
                            (= true %2))
                      true
                      false)
                   (conj other a)))
    true
    false))

(a-half-true false true)

; problem88
(defn symmetric-difference [set-a set-b]
  (loop [res #{}
         seq-all (concat (seq set-a) (seq set-b))]
    (if (empty? seq-all)
      res
      (recur (if (and (not= (get set-a (first seq-all)) nil)
                      (not= (get set-b (first seq-all)) nil))
               res
               (conj res (first seq-all)))
             (next seq-all)))))

(symmetric-difference #{1 2 3 4} #{1 3 5 6})

; problem90
(defn cartesian-product [set-a set-b]
  (loop [seq-a (seq set-a)
         seq-b (seq set-b)
         res #{}]
    (if (empty? seq-a)
      res
      (recur (next seq-a)
             seq-b
             (into res
                   (map #(vector (first seq-a) %) seq-b))))))

(cartesian-product #{:a :b} #{:c :d})

; problem95
(defn to-tree-or-not-to-tree [t]
  (if (or (not (or (seq? t)
                   (vector? t)))
           (not= 3 (count t)))
    false
    ((fn [_t]
       (loop [in-t _t
              idx 0]
         (let [f-ele (first in-t)]
           (cond
             (empty? in-t) true
             (and (= idx 0)
                  (not (or (= nil f-ele)
                          (seq? f-ele)
                          (vector? f-ele)))) (recur (next in-t)
                                                    (+ idx 1))
             (= nil f-ele) (recur (next in-t) (+ idx 1))
             (to-tree-or-not-to-tree f-ele) (recur (next in-t) (+ idx 1))
             :else false))))
     t)))

(= (to-tree-or-not-to-tree '(:a (:b nil nil) nil))
   true)
(= (to-tree-or-not-to-tree '(:a (:b nil nil)))
   false)
(= (to-tree-or-not-to-tree [1 nil [2 [3 nil nil] [4 nil nil]]])
   true)
(= (to-tree-or-not-to-tree [1 [2 nil nil] [3 nil nil] [4 nil nil]])
   false)
(= (to-tree-or-not-to-tree [1 [2 [3 [4 nil nil] nil] nil] nil])
   true)
(= (to-tree-or-not-to-tree [1 [2 [3 [4 false nil] nil] nil] nil])
   false)
(= (to-tree-or-not-to-tree '(:a nil ()))
   false)

; problem96
(defn beauty-is-symmetry [input]
  (let [input-seq (seq input)]
    (= ((fn expend-from-right [t]
          (let [res-seq (seq [])]
            (if (or (vector? t)
                    (list? t))
              (concat (conj res-seq (first (seq t)))
                    (expend-from-right (last (seq t)))
                    (expend-from-right (first (next (seq t)))))
              (conj res-seq t))))
        (last input-seq))
       ((fn expend-from-left [t]
          (let [res-seq (seq [])]
            (if (or (vector? t)
                    (list? t))
              (concat (conj res-seq (first (seq t)))
                      (expend-from-left (first (next (seq t))))
                      (expend-from-left (last (seq t))))
              (conj res-seq t))))
         (first (next input-seq))))))

(= (beauty-is-symmetry '(:a (:b nil nil) (:b nil nil)))
   true)
(= (beauty-is-symmetry '(:a (:b nil nil) nil))
   false)
(= (beauty-is-symmetry '(:a (:b nil nil) (:c nil nil)))
   false)
(= (beauty-is-symmetry [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
                          [2 [3 nil [4 [6 nil nil] [5 nil nil]]] nil]])
   true)

(= (beauty-is-symmetry [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
                          [2 [3 nil [4 [5 nil nil] [6 nil nil]]] nil]])
   false)
(= (beauty-is-symmetry [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
                          [2 [3 nil [4 [6 nil nil] nil]] nil]])
   false)

; problem97
(defn pascal-triangle [n]
  (loop [idx 1
         res [1]]
    (if (= idx n)
      res
      (recur (+ idx 1)
             (if (= idx 1)
               (conj res 1)
               ((fn [p-seq]
                  (loop [inner-res [1]
                         inner-seq p-seq]
                    (if (= 1 (count inner-seq))
                      (conj inner-res 1)
                      (recur (conj inner-res (+ (first inner-seq) (first (next inner-seq))))
                             (next inner-seq)))))
                res))))))

(= (pascal-triangle 1) [1])
(= (map pascal-triangle (range 1 6))
   [     [1]
        [1 1]
       [1 2 1]
      [1 3 3 1]
     [1 4 6 4 1]])
(= (pascal-triangle 11)
   [1 10 45 120 210 252 210 120 45 10 1])


; problem100
(defn least-common-multiple [a & more]
  (loop [seq-tmp (conj (seq more) a)
         res a]
    (if (empty? seq-tmp)
      res
      (recur (next seq-tmp)
             (/ (* res (first seq-tmp))
                ((fn gcd [a b]
                  (if (= b 0)
                    a
                    (gcd b (mod a b))))
                  res
                  (first seq-tmp)))))))


(= (least-common-multiple 2/5 1/3) 2)

(mod 1/2 1/4)

; problem107
(defn simple-closures [n]
  (let [mi n]
    (fn [x]
      (loop [inner-n mi
             res 1]
        (if (= inner-n 0)
          res
          (recur (dec inner-n)
                 (* res x)))))))

((simple-closures 2) 16)
