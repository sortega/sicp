(ns sicp.chapter1-3
  (:use [sicp.chapter1-1 :only [average]])
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


;; Exercise 1.34
;; =============

(defn f [g] (g 2))

; Signals error since 2 is not callable
;
; (f f)
; (f 2)
; (2 2) => error!


;; Half-interval method
;; ====================

(declare close-enough?)

(defn search [f neg-point pos-point]
  (let [midpoint (average neg-point pos-point)]
    (if (close-enough? neg-point pos-point)
      midpoint
      (let [test-value (f midpoint)]
        (cond (pos? test-value) (search f neg-point midpoint)
              (neg? test-value) (search f midpoint pos-point)
              :else             midpoint)))))

(defn diff [x y]
  (Math/abs (- x y)))

(defn close-enough? [x y]
  (< (diff x y) 0.001))

(defn half-interval-method [f a b]
  (let [a-value (f a)
        b-value (f b)]
    (cond (and (neg? a-value) (pos? b-value)) (search f a b)
          (and (neg? b-value) (pos? a-value)) (search f b a)
          :else (throw (Exception. "Values are not of opposite sign")))))

(def ^:dynamic tolerance 0.00001)

(defn fixed-point [f first-guess]
  (letfn [(close-enough? [v1 v2] (< (diff v1 v2) tolerance))]
    (loop [guess first-guess]
      (let [next (f guess)]
        (if (close-enough? guess next)
          next
          (recur next))))))

(defn sqrt [x]
  (fixed-point (fn [y] (average y (/ x y))) 1.0))


;; Exercise 1.35
;; =============

; 1 + 1/phi = (phi + 1) / phi =
; (3 + sqrt(5)) / (1 + sqrt(5)) =
; (3 - 3sqrt(5) + sqrt(5) - 5) / (1 - 5) =
; (-2 -2sqrt(5)) / -4 =
; (1 + sqrt(5)) / 2 = phi
; so it is a fixed point

(defn phi []
  (fixed-point #(+ 1 (/ 1 %)) 1.0))


;; Exercise 1.36
;; =============

(comment
  (defn fixed-point [f first-guess]
    (letfn [(close-enough? [v1 v2] (< (diff v1 v2) tolerance))]
      (loop [guess first-guess]
        (let [next (f guess)]
          (println (str "Guess " guess ", next guess " next))
          (if (close-enough? guess next)
            next
            (recur next))))))

  (fixed-point #(/ (Math/log 1000) (Math/log %)) 2.0))


;; Exercise 1.37
;; =============

(defn cont-frac [n-fn d-fn k]
  (->> (range k 0 -1)
    (map (juxt n-fn d-fn))
    (reduce (fn [accum [n d]] (/ n (+ d accum))) 0.0)))

; It takes 11 iterations:
;
; user=> (/ 1 (/ (inc (Math/sqrt 5)) 2))
; 0.6180339887498948
; user=> (cont-frac (fn [_] 1) (fn [_] 1) 11)
; 0.6180555555555556


;; Exercise 1.38
;; =============

(defn approx-e [k]
  (+ 2
     (cont-frac
       (fn [_] 1)
       (fn [i] (if (= (rem (dec i) 3) 1)
                 (* 2 (inc (quot i 3)))
                 1))
       k)))


;; Exercise 1.39
;; =============

(defn tan-cf [x k]
  (cont-frac
    (fn [i] (case i
              1 x
              (square x)))
    (fn [i] (dec (* 2 i)))
    k))
