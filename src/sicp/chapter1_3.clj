(ns sicp.chapter1-3
  (:use [sicp.chapter1-2 :only [gcd square prime?]]))

;; Exercise 1.29
;; =============

(defn cube [n] (* n n n))

(defn sum [term a next b]
  (if (> a b)
    0
    (+ (term a)
       (sum term (next a) next b))))

(defn integral [f a b dx]
  (letfn[(add-dx [x] (+ x dx))]
    (* (sum f (+ a (/ dx 2.0)) add-dx b)
       dx)))

(defn simpson-integral [f a b n]
  (let [h    (/ (- b a) n)
        term (fn [i]
               (let [factor (cond (or (= i 0) (= i n)) 1
                                  (zero? (mod i 2))    2
                                  :else                4)
                     x      (+ a (* i h))]
                 (* factor (f x))))]
    (* (sum term 0 inc n) (/ h 3)))) 

; user=> (simpson-integral cube 0 1 10)
; 1/4
;
; I got a perfect result with the Simpson method.


;; Exercise 1.30
;; =============

(defn sum [term a next b]
  (loop [a a, result 0.0]
    (if (> a b)
      result
      (recur (next a) (+ result (term a))))))


;; Exercise 1.31
;; =============

(defn product [term a next b]
  (loop [a a, result 1.0]
    (if (> a b)
      result
      (recur (next a) (* result (term a))))))

(comment
  (defn product [term a next b]
    (if (> a b)
      1.0
      (* (term a)
         (product term (next a) next b)))))

(defn factorial [n]
  (product identity 1 inc n))

(defn approximate-pi [terms]
  (* (product (fn [k] (/ (* k (+ k 2)) (square (inc k))))
              2 (partial + 2) (* 2 terms))
     4))

;; Exercise 1.32
;; =============

(defn accumulate [combiner null-value term a next b]
  (loop [a a, result null-value]
    (if (> a b)
      result
      (recur (next a) (combiner result (term a))))))

(comment
  (defn accumulate [combiner null-value term a next b]
    (if (> a b)
      null-value
      (combiner (term a)
                (accumulate term (next a) next b)))))

(def sum (partial accumulate + 0.0))
(def product (partial accumulate * 1.0))


;; Exercise 1.33
;; =============

(defn filtered-accumulate [combiner null-value filter term a next b]
  (loop [a a, result null-value]
    (if (> a b)
      result
      (recur (next a)
             (if (filter a)
               (combiner result (term a))
               result)))))

(defn sum-prime-squares [a b]
  (filtered-accumulate
    + 0.0
    prime? square
    a inc b))

(defn product-of-relatively-primes [n]
  (filtered-accumulate
    * 1.0
    #(= 1 (gcd n %))
    identity
    1 inc (dec n)))
