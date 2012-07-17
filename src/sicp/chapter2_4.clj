(ns sicp.chapter2-4
  (:refer-clojure :exclude [get])
  (:use [clojure.set :as s])
  (:use [sicp.chapter1-1 :only [square]])
  (:use [sicp.chapter2-3 :only [make-sum addend augend
                                make-product multiplier multiplicand
                                make-exponentiation base exponent]]))


;; Complex numbers
;; ===============

(declare real-part imag-part make-from-real-imag
         magnitude angle make-from-mag-ang)

(defn add-complex [z1 z2]
  (make-from-real-imag (+ (real-part z1) (real-part z2))
                       (+ (imag-part z1) (imag-part z2))))

(defn sub-complex [z1 z2]
  (make-from-real-imag (- (real-part z1) (real-part z2))
                       (- (imag-part z1) (imag-part z2))))

(defn mul-complex [z1 z2]
  (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                     (+ (angle z1) (angle z2))))

(defn div-complex [z1 z2]
  (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                     (- (angle z1) (angle z2))))

;; Rectangular complex numbers
;; ---------------------------

(defn sqrt [x] (Math/sqrt x))
(def real-part first)
(def imag-part second)

(defn magnitude [z]
  (sqrt (+ (square (real-part z))
           (square (imag-part z)))))

(defn angle [z]
  (Math/atan2 (imag-part z)
              (real-part z)))

(defn make-from-real-imag [x y] [x y])

(defn make-from-mag-ang [r a]
  [(* r (Math/cos a)) (* r (Math/sin a))])

;; Polar complex numbers
;; ---------------------

(defn real-part [z]
  (* (magnitude z) (Math/cos (angle z))))

(defn imag-part [z]
  (* (magnitude z) (Math/sin (angle z))))

(def magnitude first)
(def angle second)

(defn make-from-real-imag [x y]
  [(sqrt (+ (square x) (square y)))
   (Math/atan2 y x)])

(defn make-from-mag-ang [r a] [r a])

;; Type tagging
;; ============

; Detour from SICP: I'm rather using clojure metadata facility instead of
; their pair notation that is not exactly translatable to clojure.

(defn attach-tag [type-tag contents]
  (with-meta contents
             { :type type-tag}))

(defn type-tag [datum]
  (if-let [tag (:type (meta datum))]
    tag
    (throw (Exception. "Not a tagged datum"))))

(def contents identity)

; Using type tags

(defn rectangular? [z]
  (= (type-tag z) :rectangular))

(defn polar? [z]
  (= (type-tag z) :polar))

; Tagged rectagular numbers
; -------------------------

(def real-part-rectangular first)
(def imag-part-rectangular second)

(defn magnitude-rectangular [z]
  (sqrt (+ (square (real-part-rectangular z))
           (square (imag-part-rectangular z)))))

(defn angle-rectangular [z]
  (attach-tag :rectangular
              (Math/atan2 (imag-part-rectangular z)
                          (real-part-rectangular z))))

(defn make-from-real-imag-rectangular [x y] [x y])

(defn make-from-mag-ang-rectangular [r a]
  (attach-tag :rectangular
              [(* r (Math/cos a)) (* r (Math/sin a))]))

;; Tagged polar complex numbers
;; ----------------------------

(def magnitude-polar first)
(def angle-polar second)

(defn real-part-polar [z]
  (* (magnitude-polar z) (Math/cos (angle-polar z))))

(defn imag-part-polar [z]
  (* (magnitude-polar z) (Math/sin (angle-polar z))))

(defn make-from-real-imag-polar [x y]
  (attach-tag :polar
              [(sqrt (+ (square x) (square y)))
               (Math/atan2 y x)]))

(defn make-from-mag-ang-polar [r a]
  (attach-tag :polar
              [r a]))

;; Generic selectors
;; -----------------

(defn real-part [z]
  (cond (rectangular? z) (real-part-rectangular (contents z))
        (polar? z)       (real-part-polar (contents z))
        :else            (throw (Exception. "Unknown type"))))

(defn imag-part [z]
  (cond (rectangular? z) (imag-part-rectangular (contents z))
        (polar? z)       (imag-part-polar (contents z))
        :else            (throw (Exception. "Unknown type"))))

(defn magnitude [z]
  (cond (rectangular? z) (magnitude-rectangular (contents z))
        (polar? z)       (magnitude-polar (contents z))
        :else            (throw (Exception. "Unknown type"))))

(defn angle [z]
  (cond (rectangular? z) (angle-rectangular (contents z))
        (polar? z)       (angle-polar (contents z))
        :else            (throw (Exception. "Unknown type"))))

;; Some rules for newly create complex numbers

(def make-from-real-imag make-from-real-imag-rectangular)
(def make-from-mag-ang make-from-mag-ang-polar)


;; Data-directed programming
;; =========================

(declare put get)

(defn install-rectangular-package []
  (letfn [
          ;; internal procedures
          (real-part [z] (first z))
          (imag-part [z] (second z))
          (make-from-real-imag [x y] [x y])
          (magnitude [z]
            (sqrt (+ (square (real-part z))
                     (square (imag-part z)))))
          (angle [z]
            (Math/atan2 (imag-part z) (real-part z)))
          (make-from-mag-ang [r a]
            (list (* r (Math/cos a)) (* r (Math/sin a))))

          ;; interface to the rest of the system
          (tag [x] (attach-tag :rectangular x))
          ]
    (put 'real-part [:rectangular] real-part)
    (put 'imag-part [:rectangular] imag-part)
    (put 'magnitude [:rectangular] magnitude)
    (put 'angle     [:rectangular] angle)
    (put 'make-from-real-imag :rectangular
      (fn [x y] (tag (make-from-real-imag x y))))
    (put 'make-from-mag-ang :rectangular
      (fn [r a] (tag (make-from-mag-ang r a))))))

(defn install-polar-package []
  (let [
        ;; internal procedures
        magnitude           first
        angle               second
        make-from-mag-ang   (fn [r a] [r a])
        real-part           #(* (magnitude %) (Math/cos (angle %)))
        imag-part           #(* (magnitude %) (Math/sin (angle %)))
        make-from-real-imag (fn [x y]
                              (list (sqrt (+ (square x) (square y)))
                                    (Math/atan2 y x)))
        ;; interface to the rest of the system
        tag                 (partial attach-tag :polar)]
    (put 'real-part [:polar] real-part)
    (put 'imag-part [:polar] imag-part)
    (put 'magnitude [:polar] magnitude)
    (put 'angle [:polar] angle)
    (put 'make-from-real-imag :polar (comp tag make-from-real-imag))
    (put 'make-from-mag-ang :polar (comp tag make-from-mag-ang))))

(defn apply-generic [op & args]
  (let [type-tags (map type-tag args)]
    (if-let [proc (get op type-tags)]
      (apply proc (map contents args))
      (throw (Exception. "No method found")))))
        
(def real-part (partial apply-generic 'real-part))
(def imag-part (partial apply-generic 'imag-part))
(def magnitude (partial apply-generic 'magnitude))
(def angle     (partial apply-generic 'angle))
(defn make-from-real-imag [x y]
  ((get 'make-from-real-imag :rectangular) x y))
(defn make-from-mag-ang [r a]
  ((get 'make-from-mag-ang :polar) r a))

;; Exercise 2.73
;; =============

;; a) The first element of parethesized expressions is taken as the type tag.
;; That is of no use for numbers of variables as they are not lists.  For that
;; reason clojure multimethods or protocols are superior solutions that allow
;; for dispatching even discriminating null/nil values.
;;
;; b) I'll rather use multimethods to do the same more succintly.

(defn exp-type [exp _]
  (cond
    (sequential? exp) (first exp)
    (number? exp)     :number
    :else             :variable))

(defmulti deriv exp-type)

(defmethod deriv :number [_ var] 0)

(defmethod deriv :variable [exp var]
  (if (= exp var) 1 0))

(defmethod deriv '+ [exp var]
  (make-sum (deriv (addend exp) var)
            (deriv (augend exp) var)))

(defmethod deriv '* [exp var]
  (make-sum
    (make-product (multiplier exp)
                  (deriv (multiplicand exp) var))
    (make-product (deriv (multiplier exp) var)
                  (multiplicand exp))))

;; c) Exponentiation rule

(defmethod deriv '** [exp var]
  (let [b (base exp)
        e (exponent exp)]
    (make-product
      (make-product e (make-exponentiation b (dec e)))
      (deriv b var))))

;; d) Doesn't apply

;; Exercise 2.74
;; =============

(defprotocol Division
  (get-record [this name])
  (get-salary [this name]))

(defn find-employee-record [name divisions]
  (first
    (keep #(get-record % name) divisions)))

(defrecord FooDivision [records]
  Division
  (get-record [_ name] (find records name))
  (get-salary [_ name] (get-in records [name :salary])))
(def foo (FooDivision. {"John" {:salary 30000
                                :position :engineer}
                        "Mike" {:salary 40000
                                :position :architect}}))

(def bar (let [records #{["Nick" 35000 "ing"]
                         ["Bob"  45000 "des"]}]
           (reify Division
             (get-record [_ name]
               (some (fn [record]
                       (when (= (first record) name)
                         record))
                     records))
             (get-salary [this name]
               (if-let [r (get-record this name)]
                 (second r))))))
               
(comment
  (find-employee-record "Bob" [foo bar]))

;; New department databases can be added without modifying previous code ;)


;; Message passing
;; ===============

(defn make-from-real-imag [x y]
  (fn dispatch [op]
    (case op
      :real-part x
      :imag-part y
      :magnitude (sqrt (+ (square x) (square y)))
      :angle     (Math/atan2 y x)
      (throw (Exception. "Unknown op")))))

(defn apply-generic [op arg]
  (arg op))

;; Exercise 2.75
;; =============

(defn make-from-mag-ang [mag ang]
  (fn dispatch [op]
    (case op
      :real-part (* mag (Math/cos ang))
      :imag-part (* mag (Math/sin ang))
      :magnitude mag
      :angle     ang
      (throw (Exception. "Unknown op")))))

;; Exercise 2.76
;; =============

;; generic operations with explicit dispatch
;;   new types: edit all generic operations
;;   new operations: just add a new operation
;; data-directed style
;;   new types: register new type operations
;;   new operations: register new operation
;; message-passing style
;;   new types: unaffected
;;   new operations: edit all objects
;;
;; message-passing styles are the most appropriate for systems in which you
;; add types quite often while data-directed one is more suitable in the case
;; of adding new operations
