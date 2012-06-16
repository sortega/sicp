(ns sicp.chapter2-2
  (:use [sicp.chapter1-2 :only [square]]))

;; Exercise 2.17
;; =============

(defn last-pair [aseq]
  (let [r (rest aseq)]
    (if (empty? r) aseq (recur r))))


;; Exercise 2.18
;; =============

(defn rec-reverse [aseq]
  (loop [aseq aseq
         rev  ()]
    (if (empty? aseq)
      rev
      (recur (rest aseq)
             (cons (first aseq) rev)))))


;; Exercise 2.19
;; =============

; I already did that back in Exercise 1.14. It would be only renaming.

(defn count-change [amount]
  (letfn [(cc [amount coins]
            (println (str "(cc " amount "," (vec coins) ")"))
            (cond (zero? amount) 1
                  (or (neg? amount)
                      (zero? (count coins))) 0
                  :else (+ (cc amount (rest coins))
                           (cc (- amount (first coins)) coins))))]
    (cc amount [50 25 10 5 1])))

; The order of the values must be decreasing.


;; Exercise 2.20
;; =============

(defn same-parity [prototype & nums]
  (let [parity #(mod % 2)
        p      (parity prototype)]
    (cons prototype
          (filter #(= p (parity %)) nums))))


;; Exercise 2.21
;; =============

(defn square-list [items]
  (when-not (empty? items)
    (cons (square (first items)) (square-list (rest items)))))

(defn square-list [items]
  (map square items))


;; Exercise 2.22
;; =============

; At first, the result is reversed since it pops from the head of the input
; list and accumulates at the head of the answer one.  That is like dumping
; one stack into another.
;
; The second approach fails since well formed lists use the car for the list
; elements so you end with something else.


;; Exercise 2.23
;; =============

(defn for-each [f aseq]
  (when-not (empty? aseq)
    (f (first aseq))
    (recur f (rest aseq))))

