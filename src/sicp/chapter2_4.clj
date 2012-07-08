(ns sicp.chapter2-4
  (:use [clojure.set :as s])
  (:use [sicp.chapter1-1 :only [square]]))


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
