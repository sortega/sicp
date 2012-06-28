(ns sicp.chapter2-3
  (:use [sicp.chapter1-2 :only [abs]]))

(defn memq [item x]
  (cond (empty? x) false
        (= item (first x)) x
        :else (recur item (rest x))))

(comment
  (defn memq [item x]
    (some #(= item %) x)))


;; Exercise 2.53
;; =============

;; (list 'a 'b 'c) => (a b c)
;; (list (list 'george)) => ((george))
;; (rest '((x1 x2) (y1 y2))) => ((y1 y2))
;; (second '((x1 x2) (y1 y2))) => (y1 y2)
;; (seq? (car '(a short list))) => false
;; (memq 'red '((red shoes) (blue socks))) => false
;; (memq 'red '(red shoes blue socks)) => (red shoes blue socks)


;; Exercise 2.54
;; =============

(def equal? =) ; using clojure is like cheating


;; Exercise 2.55
;; =============

;; 'foo is syntax sugar for (quote foo) so ''foo is evaluated to (quote foo).
;; The first element of that list is the quote procedure.


;; Differentiation system
;; ======================

(declare variable?
         same-variable?
         sum?
         addend
         augend
         make-sum
         product?
         multiplier
         multiplicand
         make-product)

(defn deriv [exp var]
  (cond (number? exp)   0
        (variable? exp) (if (same-variable? exp var) 1 0)
        (sum? exp)      (make-sum (deriv (addend exp) var)
                                  (deriv (augend exp) var))
        (product? exp)  (make-sum
                          (make-product (multiplier exp)
                                        (deriv (multiplicand exp) var))
                          (make-product (deriv (multiplier exp) var)
                                        (multiplicand exp)))
        :else           (throw
                          (Exception. 
                            (format "unknown expression type -- DERIV: %s" 
                                    exp)))))

(defn third [x] (nth x 2))
(defn =number? [exp num]
  (and (number? exp)
       (= exp num)))
(defn operation? [op x]
  (and (seq? x)
       (= (first x) op)))

(def variable? symbol?)
(def same-variable? =)

(defn make-sum [a1 a2]
  (cond (=number? a1 0) a2
        (=number? a2 0) a1
        (and (number? a1) (number? a2)) (+ a1 a2)
        :else ['+ a1 a2]))
(def sum? (partial operation? '+))
(def addend second)
(def augend third)

(defn make-product [m1 m2]
  (cond (or (=number? m1 0) (=number? m2 0)) 0
        (=number? m1 1) m2
        (=number? m2 1) m1
        (and (number? m1) (number? m2)) (* m1 m2)
        :else ['* m1 m2]))
(def product? (partial operation? '*))
(def multiplier second)
(def multiplicand third)


;; Exercise 2.56
;; =============

(def exponentiation? (partial operation? '**))
(def base second)
(def exponent third)
(defn make-exponentiation [base exp]
  (cond (=number? base 0) 0
        (=number? exp 0) 1
        (=number? exp 1) base
        (and (number? base) (integer? exp)) (apply * (repeat exp base))
        :else ['** base exp]))

(defn deriv [exp var]
  (cond (number? exp)   0
        (variable? exp) (if (same-variable? exp var) 1 0)
        (sum? exp)      (make-sum (deriv (addend exp) var)
                                  (deriv (augend exp) var))
        (product? exp)  (make-sum
                          (make-product (multiplier exp)
                                        (deriv (multiplicand exp) var))
                          (make-product (deriv (multiplier exp) var)
                                        (multiplicand exp)))
        (exponentiation? exp)
                        (let [b (base exp)
                              e (exponent exp)]
                          (make-product
                            (make-product e (make-exponentiation b (dec e)))
                            (deriv b var)))
        :else           (throw
                          (Exception. 
                            (format "unknown expression type -- DERIV: %s" 
                                    exp)))))


;; Exercise 2.57
;; =============

(defn second-operand [[oper op1 op2 & operands]]
  (if operands
    (apply list oper op2 operands)
    op2))

(defn make-sum [& operands]
  (let [{numbers     true
         expressions false} (group-by number? operands)
        sum                 (reduce + numbers)]
    (cond (empty? expressions) sum
          (zero? sum)          (if (= 1 (count expressions))
                                 (first expressions)
                                 (apply list '+ expressions))
          :else                (apply list '+ sum expressions))))
(def augend second-operand)

(defn make-product [& operands]
  (let [{numbers     true
         expressions false} (group-by number? operands)
        product             (reduce * numbers)]
    (cond (empty? expressions) product
          (zero? product)      0
          (== product 1)       (if (= 1 (count expressions))
                                 (first expressions)
                                 (apply list '* expressions))
          :else                (apply list '* product expressions))))
(def multiplicand second-operand)


;; Exercise 2.58
;; =============

;; a) infix notation with fully parentherized expressions

(defn operation? [op x]
  (and (seq? x)
       (= (second x) op)))

(defn make-sum [a1 a2]
  (cond (=number? a1 0) a2
        (=number? a2 0) a1
        (and (number? a1) (number? a2)) (+ a1 a2)
        :else (list a1 '+ a2)))
(def sum? (partial operation? '+))
(def addend first)
(def augend third)

(defn make-product [m1 m2]
  (cond (or (=number? m1 0) (=number? m2 0)) 0
        (=number? m1 1) m2
        (=number? m2 1) m1
        (and (number? m1) (number? m2)) (* m1 m2)
        :else (list m1 '* m2)))
(def product? (partial operation? '*))
(def multiplier first)
(def multiplicand third)

;; b) infix notation with standard algebraic notation

(def precedence {'* 0
                 '+ 1})
(defn operation [x]
  (apply max-key precedence (operators x)))

(defn operators [x]
  (map second
       (partition 2 x)))

(defn operation? [op x]
  (and (seq? x)
       (= (operation x) op)))
(def sum? (partial operation? '+))
(def product? (partial operation? '*))

(defn first-operand [operator x]
  (let [op1 (take-while #(not= operator %) x)]
    (if (> (count op1) 1)
      op1
      (first op1))))

(defn second-operand [operator x]
  (let [op2 (rest (drop-while #(not= operator %) x))]
    (if (> (count op2) 1)
      op2
      (first op2))))

(def addend (partial first-operand '+))
(def augend (partial second-operand '+))
(def multiplier (partial first-operand '*))
(def multiplicand (partial second-operand '*))
