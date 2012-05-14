(ns sicp.chapter1-1)

;; Exercise 1.1
;; ============

10
; 10

(+ 5 3 4)
; 12

(- 9 1)
; 8

(/ 6 2)
; 3

(+ (* 2 4) (- 4 6))
; 6

(def a 3)
; #'sicp.chapter1/a

(def b (+ a 1))
; #'sicp.chapter1/b

(+ a b (* a b))
; 19

(= a b)
; false

(if (and (> a b) (< b (* a b)))
  b
  a)
; 3

(cond (> a b) a
      (< a b) b
      :else   -1)
; 4

(+ 2 (if (> b a) b a))
; 6

(* (cond (> a b) a
         (< a b) b
         :else   -1)
   (+ a 1))
; 16


;; Exercise 1.2
;; ============

(/ (+ 5 4 (- 2 (- 3 (+ 6 4/5))))
   (* 3 (- 6 2) (- 2 7)))


;; Exercise 1.3
;; ============

(defn sqsum-biggest-pair [& args]
  (->> (sort > args)
    (take 2)
    (map #(* % %))
    (apply +)))


;; Exercise 1.4
;; ============

(defn a-plus-abs-b [a b]
  ((if (pos? b) + -) a b))

; When b is negative, the - function is applied. Otherwise, + function does.


;; Exercise 1.5
;; ============

(defn p [] (p))

(defn order-test [x y]
  (if (zero? x)
    0
    y))

(comment
  (order-test 0 (p)))

; You get a stack overflow with applicative-order evaluation since p is
; infinitely recursive.  With normal-order evaluation 'y' variable never gets
; evaluated and you get the result instantly.


;; Square roots
;; ============

(defn square [x]
  (* x x))

(defn average [x y]
  (/ (+ x y) 2))

(defn improve [guess x]
  (average guess (/ x guess)))

(defn good-enough? [guess x]
  (< (Math/abs (- (square guess) x)) 0.001))

(defn sqrt-iter [guess x]
  (if (good-enough? guess x)
    guess
    (recur (improve guess x) x)))

(defn sqrt [x]
  (sqrt-iter 1.0 x))


;; Exercise 1.6
;; ============

(defn new-if [predicate then-clause else-clause]
  (cond predicate then-clause
        :else     else-clause))

(defn sqrt-iter2 [guess x]
  (new-if (good-enough? guess x)
    guess
    (sqrt-iter2 (improve guess x) x)))

(defn sqrt2 [x]
  (sqrt-iter2 1.0 x))

;; You get an stack-overflow error since new-if evaluates both then-clause and
;; else-clause, creating an infinite recursion.


;; Exercise 1.7
;; ============

; For very small numbers, a fixed precision of 0.001 can mean huge errors. For very 
; big ones, the algorihm might get into an infinite loop if the representation
; precision is not enough.

(defn good-enough? [guess old-guess]
  (< (Math/abs (- (/ guess old-guess) 1))
     0.001))

(defn sqrt-iter [old-guess x]
  (let [guess (improve old-guess x)]
    (if (good-enough? guess old-guess)
      guess
      (recur guess x))))


;; Exercise 1.8
;; ============

(defn improve-cube [y x]
  (/ (+ (/ x (square y))
        (* 2 y))
     3))

(defn cube-root [x]
  (loop [old-guess 1.0]
    (let [guess (improve-cube old-guess x)]
      (if (good-enough? guess old-guess)
        old-guess
        (recur guess)))))


;; Block structure
;; ===============

(defn sqrt [x]
  (letfn [(improve [guess]
            (average guess (/ x guess)))
          (good-enough? [guess old-guess]
            (< (Math/abs (- (/ guess old-guess) 1))
               0.001))
          (sqrt-iter [old-guess]
            (let [guess (improve old-guess)]
              (if (good-enough? guess old-guess)
                guess
                (recur guess))))]
    (sqrt-iter 1.0)))
