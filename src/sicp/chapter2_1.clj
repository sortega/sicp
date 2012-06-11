(ns sicp.chapter2-1
  (:use [sicp.chapter1-1 :only [average]])
  (:use [sicp.chapter1-2 :only [abs gcd square]]))

(defn make-rat [n d] [n d])
(def numer first)
(def denom second)

(defn add-rat [x y]
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(defn sub-rat [x y]
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(defn mul-rat [x y]
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(defn div-rat [x y]
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(defn equal-rat? [x y]
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(defn print-rat [x]
  (print (format "\n%d/%d" (numer x) (denom x))))

(def one-half (make-rat 1 2))
(def one-third (make-rat 1 3))


;; Exercise 2.1
;; ============

(defn make-rat [n d]
  (let [g (gcd n d)]
    [(quot n g) (quot d g)]))

; `gcd` implementation takes care of sign normalization (unintended coolness).


;; Exercise 2.2
;; ============

(declare make-point x-point y-point
         make-segment start-segment end-segment)

(defn midpoint-segment [segment]
  (let [s (start-segment segment)
        e (end-segment segment)]
    (make-point (average (x-point s) (x-point e))
                (average (y-point s) (y-point e)))))

(defn make-point ([x y] [x y]))
(def x-point first)
(def y-point second)
(defn print-point [point]
  (println (format "(%d, %d)" 
                   (x-point point) 
                   (y-point point))))

(defn make-segment 
  ([start end] [start end])
  ([x1 y1 x2 y2]
    [(make-point x1 y1) (make-point x2 y2)]))
(def start-segment first)
(def end-segment second)

(comment (print-point (midpoint-segment (make-segment 0 0 2 4))))


;; Exercise 2.3
;; ============

; Repesentation based on top-left point and dimensions
(defn make-rectangle [x1 y1 x2 y2]
  ; top-left point + (w, h)
  [(make-point (Math/min x1 x2)
               (Math/min y1 y2))
   (abs (- x1 x2))
   (abs (- y1 y2))])
(defn top-left [rect]
  (first rect))
(defn top-right [rect]
  (let [[p w _] rect]
    (make-point (+ (x-point p) w)
                (y-point p))))
(defn bottom-left [rect]
  (let [[p _ h] rect]
    (make-point (x-point p)
                (+ (y-point p) h))))
(defn bottom-right [rect]
  (let [[p w h] rect]
    (make-point (+ (x-point p) w)
                (+ (y-point p) h))))

(defn area [rect]
  (let [tl (top-left rect)
        br (bottom-right rect)]
    (* (- (x-point br) (x-point tl))
       (- (y-point br) (y-point tl)))))

(defn perimeter [rect]
  (let [tl (top-left rect)
        br (bottom-right rect)]
    (* 2
       (+ (- (x-point br) (x-point tl))
          (- (y-point br) (y-point tl))))))

; Alternative representation based on two points
(defn make-rectangle [x1 y1 x2 y2]
  (let [[xmin xmax] (if (< x1 x2) [x1 x2] [x2 x1])
        [ymin ymax] (if (< y1 y2) [y1 y2] [y2 y1])]
    [(make-point xmin ymin)
     (make-point xmax ymax)]))
(def top-left first)
(def bottom-right second)
(defn top-right [rect]
  (make-point (x-point (bottom-right rect))
              (y-point (top-left rect))))
(defn bottom-left [rect]
  (make-point (x-point (top-left rect))
              (y-point (bottom-right rect))))


;; Procedural abstraction
;; ======================

(defn kons [x y]
  (fn dispatch [m]
    (case m
      0 x
      1 y
      (throw (Exception. "Argument not 0 or 1 -- kons")))))
(defn kar [z] (z 0))
(defn kdr [z] (z 1))


;; Exercise 2.4
;; ============

(defn kons [x y]
  (fn [m] (m x y)))
(defn kar [z]
  (z (fn [x _] x)))
(defn kdr[z]
  (z (fn [_ y] y)))


;; Exercise 2.5
;; ============

(defn kons [x y]
  (apply * (concat (repeat x 2)
                   (repeat y 3))))
(defn factor-counter [d]
  (fn [z]
    (->> (iterate #(quot % d) z)
      (take-while #(zero? (rem % d)))
      count)))
(def kar (factor-counter 2))
(def kdr (factor-counter 3))


;; Exercise 2.6
;; ============

; Church numerals are no more than iterated application of a function so
; 'zero' is th identity.  Apart from that, univariated nested functions are a 
; reminiscence of the lambda calculus.

(defn church-zero [f]
  identity)

(defn church-inc [n]
  (fn [f]
    (fn [x]
      (f ((n f) x)))))

(defn church-one [f]
  (fn [x] (f x)))
(defn church-two [f]
  (fn [x] (f (f x))))

(defn church-add [n m]
  (fn [f]
    (fn [x]
      ((comp (n f) (m f)) x))))


;; Interval arithmetic
;; ===================

(declare make-interval lower-bound upper-bound)

(defn add-interval [x y]
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(defn mul-interval [x y]
  (let [p1 (* (lower-bound x) (lower-bound y))
        p2 (* (lower-bound x) (upper-bound y))
        p3 (* (upper-bound x) (lower-bound y))
        p4 (* (upper-bound x) (upper-bound y))]
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(defn div-interval [x y]
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))


;; Exercise 2.7
;; ============

(def make-interval vector)
(def lower-bound first)
(def upper-bound second)


;; Exercise 2.8
;; ============

(defn sub-interval [x y]
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))


;; Exercise 2.9
;; ============

; (a ~ wa) + (b ~ wb) = (a+b ~ (wa+wb))
; (a ~ wa) - (b ~ wb) = (a-b ~ (wa+wb))
; (a ~ wa) * (b ~ wb) = (a*b ~ ???)


;; Exercise 2.10
;; =============

(defn in-interval? [interval x]
  (<= (lower-bound interval) x (upper-bound interval)))

(defn div-interval [x y]
  (if (in-interval? y 0)
    (throw (Exception. "Cannot divide by 0"))
    (mul-interval x
                  (make-interval (/ 1.0 (upper-bound y))
                                 (/ 1.0 (lower-bound y))))))


;; Exercise 2.11
;; =============

(defn interval-sign [interval]
  (cond
    (neg? (upper-bound interval)) :neg
    (>= (lower-bound interval) 0) :pos
    :else                         :mix))

(defn mul-interval [x y]
  (let [a (lower-bound x)
        b (upper-bound x)
        c (lower-bound y)
        d (upper-bound y)]
    (case [(interval-sign x) (interval-sign y)]
      [:pos :pos] (make-interval (* a c) (* b d))
      [:pos :mix] (make-interval (* b c) (* b d))
      [:pos :neg] (make-interval (* b c) (* a d))
      [:mix :pos] (make-interval (* a d) (* b d))
      [:mix :mix] (let [p1 (* a c)
                        p2 (* a d)
                        p3 (* b c)
                        p4 (* b d)]
                    (make-interval (min p1 p2 p3 p4)
                                   (max p1 p2 p3 p4)))
      [:mix :neg] (make-interval (* b c) (* a c))
      [:neg :pos] (make-interval (* a d) (* b c))
      [:neg :mix] (make-interval (* a d) (* a c))
      [:neg :neg] (make-interval (* b d) (* a c)))))


;; Selectors for center-width
;; ==========================

(defn make-center-width [c w]
  (make-interval (- c w) (+ c w)))
(defn center [i]
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(defn width [i]
  (/ (- (upper-bound i) (lower-bound i)) 2))


;; Exercise 2.12
;; =============

(defn make-center-percent [c p]
  (let [factor (* c (/ p 100))]
    (make-interval (- c factor) (+ c factor))))
(defn percent [i]
  (* (/ (width i) (abs (center i))) 100))
