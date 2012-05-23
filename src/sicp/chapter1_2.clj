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
  (loop [a 1N
         b 0N
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
(defn hlv [x] (quot x 2))

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
; T_pq (a, b) = (bq + aq + ap, 
;                bp + aq)
; T_pq^2(a, b) = ((bp + aq)q + (bq + aq + ap)q + (bq + aq + ap)p,
;                 (bp + aq)p + (bq + aq + ap)q)
;              = (b(2pq+qq) + a(2pq+qq) + a(pp+qq),
;                 b(pp+qq)  + a(2pq+qq))
;              = T_(p^2+q^2),(2pq+q^2)             
; (T_01 o T_pq)(a b) = T_pq(a+b, a)
;                    = (aq + (a+b)q + (a+b)p, 
;                       ap + (a+b)q)
;                    = (b(p+q) + a(p+q) + aq,
;                       bq + a(p+q))
;                    = T_q,p+q(a, b)

(defn fast-fib [n]
  (loop [a 1N, b 0N, p 0N, q 1N, count n]
    (cond (zero? count) b
          (even? count) (recur a b
                               (+ (square p) (square q))
                               (+ (* 2 p q)  (square q)) 
                               (hlv count))
          :else         (recur (+ (* b q) (* a q) (* a p))
                               (+ (* b p) (* a q))
                               p q (dec count)))))


;; Exercise 1.20
;; =============

(defn gcd [a b]
  (if (zero? b)
    a
    (recur b (mod a b))))

; Applicative order evaluation:
;
; (gcd 206 40)
; (gcd 40 (mod 206 40)
; (gcd 40 6)
; (gcd 6 (mod 40 6))
; (gcd 6 4)
; (gcd 4 (mod 6 4))
; (gcd 4 2)
; (gcd 2 (mod 4 2))
; (gcd 2 0)
; 2
;
; Normal order evaluation:
;
; (gcd 206 40)
; (if (zero? 40) 206 (gcd 40 (mod 206 40)))
; (gcd 40 (mod 206 40))
; (if (zero? (mod 206 40)) 40 (gcd (mod 206 40) (mod 40 (mod 206 40))))
; (if (zero? 6) 40 (gcd (mod 206 40) (mod 40 (mod 206 40))))
; (gcd (mod 206 40) (mod 40 (mod 206 40)))
; (if (zero? (mod 40 (mod 206 40))) (mod 206 40) (gcd (mod 40 (mod 206 40)) (mod (mod 206 40) (mod 40 (mod 206 40)))))
; (if (zero? (mod 40 6)) (mod 206 40) (gcd (mod 40 (mod 206 40)) (mod (mod 206 40) (mod 40 (mod 206 40)))))
; (if (zero? 4) (mod 206 40) (gcd (mod 40 (mod 206 40)) (mod (mod 206 40) (mod 40 (mod 206 40)))))
; (gcd (mod 40 (mod 206 40)) (mod (mod 206 40) (mod 40 (mod 206 40))))
; (if (zero? (mod (mod 206 40) (mod 40 (mod 206 40)))) (mod 40 (mod 206 40)) (gcd (mod (mod 206 40) (mod 40 (mod 206 40))) (mod (mod 40 (mod 206 40)) (mod (mod 206 40) (mod 40 (mod 206 40))))))
; (if (zero? (mod 6 (mod 40 (mod 206 40)))) (mod 40 (mod 206 40)) (gcd (mod (mod 206 40) (mod 40 (mod 206 40))) (mod (mod 40 (mod 206 40)) (mod (mod 206 40) (mod 40 (mod 206 40))))))
; (if (zero? (mod 6 (mod 40 6))) (mod 40 (mod 206 40)) (gcd (mod (mod 206 40) (mod 40 (mod 206 40))) (mod (mod 40 (mod 206 40)) (mod (mod 206 40) (mod 40 (mod 206 40))))))
; (if (zero? (mod 6 4)) (mod 40 (mod 206 40)) (gcd (mod (mod 206 40) (mod 40 (mod 206 40))) (mod (mod 40 (mod 206 40)) (mod (mod 206 40) (mod 40 (mod 206 40))))))
; (if (zero? 2) (mod 40 (mod 206 40)) (gcd (mod (mod 206 40) (mod 40 (mod 206 40))) (mod (mod 40 (mod 206 40)) (mod (mod 206 40) (mod 40 (mod 206 40))))))
; (gcd (mod (mod 206 40) (mod 40 (mod 206 40))) (mod (mod 40 (mod 206 40)) (mod (mod 206 40) (mod 40 (mod 206 40)))))
; (if (zero? (mod (mod 40 (mod 206 40)) (mod (mod 206 40) (mod 40 (mod 206 40))))) (mod (mod 206 40) (mod 40 (mod 206 40))) (gcd (mod (mod 40 (mod 206 40)) (mod (mod 206 40) (mod 40 (mod 206 40)))) (mod (mod (mod 206 40) (mod 40 (mod 206 40))) (mod (mod 40 (mod 206 40)) (mod (mod 206 40) (mod 40 (mod 206 40)))))))
; (if (zero? (mod (mod 40 6) (mod (mod 206 40) (mod 40 (mod 206 40))))) (mod (mod 206 40) (mod 40 (mod 206 40))) (gcd (mod (mod 40 (mod 206 40)) (mod (mod 206 40) (mod 40 (mod 206 40)))) (mod (mod (mod 206 40) (mod 40 (mod 206 40))) (mod (mod 40 (mod 206 40)) (mod (mod 206 40) (mod 40 (mod 206 40)))))))
; (if (zero? (mod 4 (mod (mod 206 40) (mod 40 (mod 206 40))))) (mod (mod 206 40) (mod 40 (mod 206 40))) (gcd (mod (mod 40 (mod 206 40)) (mod (mod 206 40) (mod 40 (mod 206 40)))) (mod (mod (mod 206 40) (mod 40 (mod 206 40))) (mod (mod 40 (mod 206 40)) (mod (mod 206 40) (mod 40 (mod 206 40)))))))
; (if (zero? (mod 4 (mod 6 (mod 40 (mod 206 40))))) (mod (mod 206 40) (mod 40 (mod 206 40))) (gcd (mod (mod 40 (mod 206 40)) (mod (mod 206 40) (mod 40 (mod 206 40)))) (mod (mod (mod 206 40) (mod 40 (mod 206 40))) (mod (mod 40 (mod 206 40)) (mod (mod 206 40) (mod 40 (mod 206 40)))))))
; (if (zero? (mod 4 (mod 6 (mod 40 6)))) (mod (mod 206 40) (mod 40 (mod 206 40))) (gcd (mod (mod 40 (mod 206 40)) (mod (mod 206 40) (mod 40 (mod 206 40)))) (mod (mod (mod 206 40) (mod 40 (mod 206 40))) (mod (mod 40 (mod 206 40)) (mod (mod 206 40) (mod 40 (mod 206 40)))))))
; (if (zero? (mod 4 (mod 6 4))) (mod (mod 206 40) (mod 40 (mod 206 40))) (gcd (mod (mod 40 (mod 206 40)) (mod (mod 206 40) (mod 40 (mod 206 40)))) (mod (mod (mod 206 40) (mod 40 (mod 206 40))) (mod (mod 40 (mod 206 40)) (mod (mod 206 40) (mod 40 (mod 206 40)))))))
; (if (zero? (mod 4 2)) (mod (mod 206 40) (mod 40 (mod 206 40))) (gcd (mod (mod 40 (mod 206 40)) (mod (mod 206 40) (mod 40 (mod 206 40)))) (mod (mod (mod 206 40) (mod 40 (mod 206 40))) (mod (mod 40 (mod 206 40)) (mod (mod 206 40) (mod 40 (mod 206 40)))))))
; (if (zero? 0) (mod (mod 206 40) (mod 40 (mod 206 40))) (gcd (mod (mod 40 (mod 206 40)) (mod (mod 206 40) (mod 40 (mod 206 40)))) (mod (mod (mod 206 40) (mod 40 (mod 206 40))) (mod (mod 40 (mod 206 40)) (mod (mod 206 40) (mod 40 (mod 206 40)))))))
; (mod (mod 206 40) (mod 40 (mod 206 40)))
; (mod 6 (mod 40 (mod 206 40)))
; (mod 6 (mod 40 6))
; (mod 6 4)
; 2
;
; Normal evaluation: 18 mod's
; Applicative evaluation: 4 mod's


;; Primality
;; =========

(defn divides? [a b] (zero? (mod b a)))

(defn smallest-divisor [n]
  (loop [divisor 2]
    (cond (> (square divisor) n) n
          (divides? divisor n)   divisor
          :else                  (recur (inc divisor)))))

(defn prime? [n]
  (= n (smallest-divisor n)))

(defn expmod [base exp m]
  (cond (zero? exp) 1
        (even? exp) (mod (square (expmod base (hlv exp) m)) m)
        :else       (mod (* base (expmod base (dec exp) m)) m)))

(defn log2 [n]
  (/ (Math/log n)
     (Math/log 2)))

(defn rand-bigint [n]
  (let [bits (inc (int (Math/round (log2 n))))
        rnd  (java.util.Random.)]
    (loop []
      (let [r (BigInteger. bits rnd)]
        (if (< r n)
          r
          (recur))))))

(defn fermat-test [n]
  (let [r (inc (rand-bigint (dec n)))]
    (= (expmod r n n) r)))

(defn test-times [test-fn]
  (fn [n times]
    (every? identity
            (for [_ (range times)] (test-fn n)))))

(def fast-prime?
  (test-times fermat-test))


;; Exercise 1.21
;; =============

(map smallest-divisor [199 1999 19999])
; (199 1999 7)


;; Exercise 1.22
;; =============

(defn runtime [] (System/currentTimeMillis))

(defn timed-prime? [n]
  (let [start (runtime)
        prime (prime? n)]
    [prime n (- (runtime) start)]))

(defn search-for-primes [a b]
  (->> (range a (inc b) 2)
    (map timed-prime?)
    (filter first)
    (map rest)))

(take 3 (search-for-primes 1001 2001))
; ((1009 0) (1013 0) (1019 0))
(take 3 (search-for-primes 10001 20001))
; ((10007 0) (10009 0) (10037 0))
(take 3 (search-for-primes 100001 200001))
; ((100003 0) (100019 1) (100043 0))
(take 3 (search-for-primes 10000001 20000001))
; ((10000019 1) (10000079 1) (10000103 1))
(take 3 (search-for-primes 10000000001 20000000001))
; ((10000000019 9) (10000000033 12) (10000000061 9))

; It doesn't fit very well on the O(sqrt(n)) model but it's understantable 
; since my machine is way more powerful than the average 80's computer and 
; this examples are very small.


;; Exercise 1.23
;; =============

(defn nexto [n]
  (if (= n 2)
    3
    (+ 2 n)))

(defn smallest-divisor [n]
  (loop [divisor 2]
    (cond (> (square divisor) n) n
          (divides? divisor n)   divisor
          :else                  (recur (nexto divisor)))))

(take 3 (search-for-primes 1001 2001))
; ((1009 0) (1013 0) (1019 0))
(take 3 (search-for-primes 10001 20001))
; ((10007 0) (10009 0) (10037 0))
(take 3 (search-for-primes 100001 200001))
; ((100003 1) (100019 0) (100043 0))
(take 3 (search-for-primes 10000001 20000001))
; ((10000019 4) (10000079 3) (10000103 2))
(take 3 (search-for-primes 10000000001 20000000001))
; ((10000000019 12) (10000000033 6) (10000000061 6))

; I think the effect is below measurement so I cannot compare.


;; Exercise 1.24
;; =============

(defn timed-prime? [n]
  (let [start (runtime)
        prime (fast-prime? n 100)]
    [prime n (- (runtime) start)]))

; user=> (take 3 (search-for-primes 1001 2001))
; ((1009 2) (1013 2) (1019 2))
; user=> (take 3 (search-for-primes 10001 20001))
; ((10007 2) (10009 3) (10037 4))
; user=> (take 3 (search-for-primes 100001 200001))
; ((100003 2) (100019 3) (100043 2))
; user=> (take 3 (search-for-primes 10000001 20000001))
; ((10000019 2) (10000079 3) (10000103 2))
; user=> (take 3 (search-for-primes 10000000001 20000000001))
; ((10000000019 4) (10000000033 3) (10000000061 3))
;
; Computation time *does* grows more slowy!


;; Exercise 1.25
;; =============

(defn expmod2 [base exp m]
  (mod (fast-exp base exp) m))

; It works if you use multiple precision numbers as base but that's 
; slower than plain old numbers. (About 10 times slower).


;; Exercise 1.26
;; =============

; Reasoner has a tree-recursive expmod implementation since every
; non-base case makes two recursive calls.  Using the master method 
; to estimate complexity (http://en.wikipedia.org/wiki/Master_theorem):
;
; T(n) <= 2T(n/2) + O(1)
;
; So it is O(n^log_2(2)) = O(n)

;; Exercise 1.27
;; =============

(def carmichaels [561 1105 1729 2465 2821 6601])

(defn carmichaelic? [n]
  (every? #(= (expmod % n n) %)
          (range 1 n)))

; user=> (remove prime? (filter carmichaelic? (range 7000)))
; (561 1105 1729 2465 2821 6601)


;; Exercise 1.28
;; =============

(defn abs [n]
  (if (neg? n) (- n) n))

(defn expmod0
  "Returns 0 when non-trivial square roots of 1 are found"
  [base exp m]
  (cond (zero? exp) 1
        (even? exp) (let [root (expmod0 base (hlv exp) m)
                          res  (mod (square root) m)]
                      (if (and (= res 1)
                               (not= (abs root) 1))
                        0
                        res))
        :else       (mod (* base (expmod0 base (dec exp) m)) m)))

(defn miller-rabin-test [n]
  (let [r (inc (rand-bigint (dec n)))]
    (= (expmod0 r n n) r)))

(def miller-rabin-prime?
  (test-times miller-rabin-test))

; user=> (map miller-rabin-prime? carmichaels (repeat 100))
; (false false false false false false)
