(ns sicp.chapter1-2)

;; Exercise 1.9
;; ============

(defn plus [a b]
  (if (zero? a)
    b
    (inc (plus (dec a) b))))

; (plus 4 5)
; (inc (plus 3 5))
; (inc (inc (plus 2 5)))
; (inc (inc (inc (plus 1 5))))
; (inc (inc (inc (inc (plus 0 5)))))
; (inc (inc (inc (inc 5))))
; (inc (inc (inc 6)))
; (inc (inc 7))
; (inc 8)
; 9

(defn plus-tco [a b]
  (if (zero? a)
    b
    (recur (dec a) (inc b))))

; (plus-tco 4 5)
; (plus-tco 3 6)
; (plus-tco 2 7)
; (plus-tco 1 8)
; (plus-tco 0 9)
; 9


;; Exercise 1.10
;; =============

(defn A [x y]
  (cond (zero? y) 0
        (zero? x) (* 2 y)
        (= y 1)   2
        :else     (recur (dec x)
                         (A x (dec y)))))

(A 1 10)
; 1024

(A 2 4)
; 65536

(A 3 3)
; 65536

(defn f [n] (A 0 n))
; f(n) = 2*n

(defn g [n] (A 1 n))
; g(n) = | 0,   if n=0
;        | 2^n, if n>0

(defn h [n] (A 2 n))
; h(n) = | 0         if n=0
;        | 2^h(n-1)  if n>0

(defn k [n] (* 5 n n))
; k(n) = 5n^2

;; Linear fibonacci
;; ================

(defn fib [n]
  (loop [a 1 
         b 0
         n n]
    (if (zero? n)
      b
      (recur (+ a b) a (dec n)))))


;; Exercise 1.11
;; =============

(defn f-rec [n]
  (if (< n 3)
    n
    (+ (f-rec (dec n))
       (* 2 (f-rec (- n 2)))
       (* 3 (f-rec (- n 3))))))

(defn f-iter [n]
  (loop [a 2
         b 1
         c 0
         n n]
    (if (zero? n)
      c
      (recur (+ a (* 2 b) (* 3 c)) a b (dec n)))))


;; Exercise 1.12
;; =============

(defn pascal [n]
  (if (= n 1)
    [1]
    (let [prev (pascal (dec n))]
      (vec (map + (cons 0 prev)
                  (conj prev 0))))))


;; Exercise 1.13
;; =============

; 1. Fib(n) is the closest integer to Fi^n/sqrt(5)
; iff Fib(n) = (Fi^n - fi^n)/sqrt(5)
; where Fi = (1 + sqrt(5))/2
;       fi = (1 - sqrt(5))/2
;
; 2. Fib(0) = 0
;    (Fi^0 - fi^0)/sqrt(5) = 0
;    True for the base case 0!
; 
; 3. Fib(1) = 1
;    (Fi^1 - fi^1)/sqrt(5) =
;    ((1 + sqrt(5)) - (1 - sqrt(5))) / (2 * sqrt(5)) =
;    (2 * sqrt(5)) / (2 * sqrt(5)) = 1
;    True for the base case 1!
;
; 4. Fib(n) = Fib(n-1) + Fib(n-2) =
;    (Fi^(n-1) - fi^(n-1) + Fi^(n-2) - fi^(n-2))/sqrt(5) =
;    [(2/(1+sqrt(5)) + 4/(1+sqrt(5))^2)*Fi^n -
;     (2/(1-sqrt(5)) + 4/(1-sqrt(5))^2)*fi^n] / sqrt(5) =
;    (Fi^n - fi^n)/sqrt(5) = 0 iff 2/(1+sqrt(5)) + 4/(1+sqrt(5))^2 = 1,
;                                  2/(1-sqrt(5)) + 4/(1-sqrt(5))^2 = 1
;    
; 4.1. 2/(1+sqrt(5)) + 4/(1+sqrt(5))^2 =
;      (2 + 2sqrt(5) + 4)/(1+sqrt(5))^2 =
;      (6 + 2sqrt(5)) / (1 + 2sqrt(5) + 5) =
;      (6 + 2sqrt(5)) / (6 + 2sqrt(5)) = 1
;      qed
;
; 4.2. 2/(1-sqrt(5)) + 4/(1-sqrt(5))^2 =
;      (2 - 2sqrt(5) + 4)/(1-sqrt(5))^2 =
;      (6 - 2sqrt(5)) / (1 - 2sqrt(5) + 5) =
;      (6 - 2sqrt(5)) / (6 - 2sqrt(5)) = 1
;      qed


;; Exercise 1.14
;; =============

(defn count-change [amount]
  (letfn [(cc [amount coins]
            (println (str "(cc " amount "," (vec coins) ")"))
            (cond (zero? amount) 1
                  (or (neg? amount)
                      (zero? (count coins))) 0
                  :else (+ (cc amount (rest coins))
                           (cc (- amount (first coins)) coins))))]
    (cc amount [50 25 10 5 1])))

;; See change.dot and change.png
;; It has an exponential order of growth both in time and space.


;; Exercise 1.15
;; =============
(defn sine [angle counter]
  (swap! counter inc)
  (letfn [(cube [n] (* n n n))
          (p [x] (- (* 3 x) (* 4 (cube x))))]
    (if (< (Math/abs angle) 0.1)
      angle
      (p (sine (/ angle 3.0) counter)))))

(comment
  (def invocations (atom 0))
  (sine 12.15 invocations)
  ; 6
  )

; Growth in space and time is logarithmic


;; Exercise 1.16
;; =============

(defn square [x] (* x x))

(defn exp [b n]
  (loop [n n
         product 1]
    (if (zero? n)
      product
      (recur (dec n) (* product b)))))

(defn fast-exp [b n]
  (cond (zero? n) 1
        (even? n) (square (fast-exp b (/ n 2)))
        :else     (* b (fast-exp b (dec n)))))

(defn fast-iter-exp [b n]
  (loop [a 1, b b, n n]
    (cond (zero? n) a
          (even? n) (recur a (square b) (/ n 2))
          :else     (recur (* a b) b (dec n)))))


;; Exercise 1.17
;; =============

(defn dbl [x] (* x 2))
(defn hlv [x] (/ x 2))

(defn fast* [a b]
  (cond (zero? b) 0
        (even? b) (fast* (dbl a) (hlv b))
        :else     (+ a (fast* a (dec b)))))


;; Exercise 1.18
;; =============

(defn fast-iter* [a b]
  (loop [a a
         b b
         sum 0]
    (cond (zero? b) sum
          (even? b) (recur (dbl a) (hlv b) sum)
          :else     (recur a (dec b) (+ sum a)))))


;; Exercise 1.19
;; =============

; T(a, b) = (a+b, b)
; T^n(0, 1) = (Fib(n+1), Fib(n))
;
; T = T_0,1
; T_pq (a, b) = (bq + aq + ap, bp + aq)
; T_pq^2(a, b) = ((bp + aq)q + (bq + aq + ap)q + (bq + aq + ap)p,
;                 (bp + aq)p + (bq + aq + ap)q)
