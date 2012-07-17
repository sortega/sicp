(ns sicp.chapter2-3
  (:use [clojure.set :as s])
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

(comment
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
)

;; b) infix notation with standard algebraic notation
(comment
  (def precedence {'* 0
                  '+ 1})

  (defn operators [x]
    (map second (partition 2 x)))

  (defn operation [x]
    (apply max-key precedence (operators x)))


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
)

;; Unordered lists as sets
;; =======================

(defn element-of-set? [x set]
  (cond (empty? set) false
        (= x (first set)) true
        :else (recur x (rest set))))

(defn adjoin-set [x set]
  (if (element-of-set? x set)
    set
    (cons x set)))

(defn intersection-set [set1 set2]
  (cond (or (empty? set1) (empty? set2)) '()
        (element-of-set? (first set1) set2)
          (cons (first set1)
                (intersection-set (rest set1) set2))
        :else (recur (rest set1) set2)))


;; Exercise 2.59
;; =============

(defn union-set [set1 set2]
  (loop [set   set1
         union set2]
    (cond (empty? set) union
          (element-of-set? (first set) union)
            (recur (rest set) union)
          :else (recur (rest set) (cons (first set) union)))))


;; Exercise 2.60
;; =============

; Only adjoint-set and union-set change
(comment
  (def adjoin-set cons)
  (def union-set concat))

; Performance of adjoin-set is increased, it depends of the case for union-set
; and it is much worse in every other case.  It may exists an application in
; which this is preferable to the previous version but it would be very
; unusal.


;; Ordered lists as sets
;; =====================

(defn element-of-set? [x set]
  (cond (empty? set)      false
        (= x (first set)) true
        (< x (first set)) false
        :else             (recur x (rest set))))

(defn intersection-set [set1 set2]
  (if (or (empty? set1) (empty? set2))
    '()
    (let [x1 (first set1)
          x2 (first set2)]
      (cond (= x1 x2) (cons x1 (intersection-set (rest set1)
                                                 (rest set2)))
            (< x1 x2) (recur (rest set1) set2)
            (< x2 x1) (recur set1 (rest set2))))))


;; Exercise 2.61
;; =============

(defn adjoin-set [x set]
  (cond (empty? set)      (list x)
        (= x (first set)) set
        (< x (first set)) (cons x set)
        :else             (cons (first set) (adjoin-set x (rest set)))))


;; Exercise 2.62
;; =============

(defn union-set [set1 set2]
  (loop [set1 (seq set1)
         set2 (seq set2) 
         union []]
    (cond (empty? set1) (into union set2)
          (empty? set2) (into union set1)
          :else (let [[p1 & r1] set1
                      [p2 & r2] set2]
                  (if (< p1 p2)
                    (recur r1 set2 (conj union p1))
                    (recur set1 r2 (conj union p2)))))))

;; Trees as sets
;; =============

(def entry first)
(def left-branch second)
(def right-branch third)
(defn make-tree [entry left right]
  (list entry left right))


(defn element-of-set? [x set]
  (cond (empty? set) false
        (= x (entry set)) true
        (< x (entry set))
          (recur x (left-branch set))
        (> x (entry set))
          (recur x (right-branch set))))

(defn adjoin-set [x set]
  (cond (empty? set) (make-tree x () ())
        (= x (entry set)) set
        (< x (entry set))
          (make-tree (entry set)
                     (adjoin-set x (left-branch set))
                     (right-branch set))
        (> x (entry set))
          (make-tree (entry set)
                     (left-branch set)
                     (adjoin-set x (right-branch set)))))


;; Exercise 2.63
;; =============

(defn tree->list-1 [tree]
  (if (empty? tree)
    ()
    (concat (tree->list-1 (left-branch tree))
            (cons (entry tree)
                  (tree->list-1 (right-branch tree))))))

(defn tree->list-2 [tree]
  (letfn [(copy-to-list [tree result-list]
            (if (empty? tree)
              result-list
              (recur (left-branch tree)
                     (cons (entry tree)
                           (copy-to-list (right-branch tree)
                                         result-list)))))]
    (copy-to-list tree ())))

;; a) Both produces the same result: and ordered list.
;; b) Both have tree recursion but the second one can take advantage of TCO
;; for one of them.


;; Exercise 2.64
;; =============

(defn partial-tree [elts n]
  (if (zero? n)
    [() elts]
    (let [left-size (quot (dec n) 2)
          [left-tree non-left-elts]
              (partial-tree elts left-size)
          right-size (- n (inc left-size))
          this-entry (first non-left-elts)
          [right-tree remaining-elts]
              (partial-tree (rest non-left-elts) right-size)]
      [(make-tree this-entry left-tree right-tree)
       remaining-elts])))

(defn list->tree [elements]
  (first (partial-tree elements (count elements))))

;; Using split-at is way simpler:

(defn list->tree2 [elts]
  (if (empty? elts)
    ()
    (let [n (count elts)
          left-size (quot (dec n) 2)
          right-size (- n (inc left-size))
          [left-elts [this-entry & right-elts]] (split-at left-size elts)]
      (make-tree this-entry
                 (list->tree2 left-elts)
                 (list->tree2 right-elts)))))

;; a) partial-tree decomposes into three parts the elts list: left, this-entry
;;    and right. The number of elements are chosen to have a balanced tree.
;; b) using the master method we can now it takes linear time:
;; 
;;    T(n) = a*T(n/b) + O(n^d), a >= 1, b > 1
;; 
;;    a = 2, b = 2, d = 0
;;
;;    T(n) = O(n^log_2(2)) = O(n)


;; Exercise 2.65
;; =============

(defn union-set [set1 set2]
  (list->tree2
    (loop [list1 (tree->list-1 set1)
           list2 (tree->list-1 set2)
           union []]
      (cond (empty? list1) (concat union list2)
            (empty? list2) (concat union list1)
            :else (let [[el1 & rest1] list1
                        [el2 & rest2] list2]
                    (cond (= el1 el2) (recur rest1 rest2 (conj union el1))
                          (< el1 el2) (recur rest1 list2 (conj union el1))
                          :else       (recur list1 rest2 (conj union el2))))))))

(defn intersection-set [set1 set2]
  (list->tree2
    (loop [list1        (tree->list-1 set1)
           list2        (tree->list-1 set2)
           intersection []]
      (if (or (empty? list1) (empty? list2))
        intersection
        (let [[el1 & rest1] list1
              [el2 & rest2] list2]
          (cond (= el1 el2) (recur rest1 rest2 (conj intersection el1))
                (< el1 el2) (recur rest1 list2 intersection)
                :else       (recur list1 rest2 intersection)))))))


;; Exercise 2.66
;; =============

(declare get-key)

(defn lookup [given-key set-of-records]
  (if-let [elem (first set-of-records)]
    (if (= given-key (get-key elem))
      elem
      (recur given-key (rest set-of-records)))))

(defn lookup [given-key set-of-records]
  (when-not (empty? set-of-records)
    (let [element (entry set-of-records)
          k       (get-key element)]
      (cond (= k given-key) element
            (< k given-key) (recur given-key (left-branch set-of-records))
            :else           (recur given-key (right-branch set-of-records))))))


;; Huffman trees using multimethods
;; ================================

(defmulti symbols :type)
(defmulti weight :type)

; Leaves

(defn make-leaf [sym weight]
  {:type   :leaf
   :symbol sym
   :weight weight})

(defn leaf? [object]
  (= (:type object) :leaf))

(def symbol-leaf :symbol)
(def weight-leaf :weight)

(defmethod symbols :leaf [x]
  (sorted-set (:symbol x)))

(defmethod weight :leaf [x]
  (:weight x))

; Branches

(defn make-code-tree [left right]
  {:type :branch
   :left left
   :right right})

(def left-branch :left)
(def right-branch :right)

(defmethod symbols :branch [{:keys [left right]}]
  (s/union (symbols left)
           (symbols right)))

(defmethod weight :branch [{:keys [left right]}]
  (+ (weight left) (weight right)))

; Decoding

(defn choose-branch [bit branch]
  (case bit
    0 (left-branch branch)
    1 (right-branch branch)
    (throw (Exception. (str "bad bit -- CHOOSE-BRANCH --" bit)))))

(defn decode [bits tree]
  (letfn [(decode-1 [bits current-branch]
            (if (empty? bits)
              ()
              (let [next-branch (choose-branch (first bits) current-branch)]
                (if (leaf? next-branch)
                  (cons (symbol-leaf next-branch)
                        (decode-1 (rest bits) tree))
                  (recur (rest bits) next-branch)))))]
    (decode-1 bits tree)))

(comment
  (defn adjoin-set [x set]
    (cond (empty? set) [x]
          (< (weight x) (weight (first set))) (cons x set)
          :else (cons (first set)
                      (adjoin-set x (rest set)))))

  (defn make-leaf-set [pairs]
    (if (empty? pairs)
      '()
      (let [pair (first pairs)]
        (adjoin-set (make-leaf (first pair)    ; symbol
                               (second pair))  ; frequency
                    (make-leaf-set (rest pairs))))))
)

(defn make-leaf-set [pairs]
  (map
    #(apply make-leaf %)
    (sort-by second pairs)))


;; Exercise 2.67
;; =============

(def sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                    (make-leaf 'B 2)
                    (make-code-tree (make-leaf 'D 1)
                                    (make-leaf 'C 1)))))

(def sample-message [0 1 1 0 0 1 0 1 0 1 1 1 0])

; sicp.chapter2-3=> (decode sample-message sample-tree)
; (A D A B B C A)


;; Exercise 2.68
;; =============

(defn encode-symbol [sym tree]
  (loop [branch tree
         code   []]
    (let [left  (left-branch branch)
          right (right-branch branch)]
      (cond (and (contains? (symbols branch) sym) (leaf? branch)) code
            (contains? (symbols left) sym)        (recur left (conj code 0))
            (contains? (symbols right) sym)       (recur right (conj code 1))
            :else nil))))

(defn encode [message tree]
  (mapcat #(encode-symbol % tree)
          message))


;; Exercise 2.69
;; =============

(defn merge-lightest-trees [sorted-trees]
  (let [[t1 t2 & ts] sorted-trees
        merged       (make-code-tree t1 t2)]
    (sort-by weight
             (cons merged ts))))

(defn succesive-merge [leaves]
  (->> (iterate merge-lightest-trees leaves)
    (drop-while #(> (count %) 1))
    first
    first))

(defn generate-huffman-tree [pairs]
  (succesive-merge (make-leaf-set pairs)))


;; Exercise 2.70
;; =============

(def rock-code (generate-huffman-tree [["A" 2] ["NA" 16] ["BOOM" 1] ["SHA" 3]
                                       ["GET" 2] ["YIP" 9] ["JOB" 2] ["WAH" 1]]))

(comment
  (encode ["GET" "A" "JOB"
           "SHA" "NA" "NA" "NA" "NA" "NA" "NA" "NA" "NA"
           "GET" "A" "JOB"
           "SHA" "NA" "NA" "NA" "NA" "NA" "NA" "NA" "NA"
           "SHA" "BOOM"] rock-code)
  (1 1 1 1 0 1 1 0 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 1 1 1 1 0 1 1 0 1 1 1 1
   1 1 1 1 1 0 0 0 0 0 0 0 0 0 1 1 1 0 1 1 0 0 0)
)

; It is encoded in 61 bits with the variable length encoding versus 78 bits of
; fixed width one (3 bits x 26 characters).


;; Exercise 2.71
;; =============

;; In that case Huffman tree looks like a linked list. The most frequent
;; symbol is encoded by one digit and the least by n-1.



;; Exercise 2.72
;; =============

;; The order of growth depends on the relative symbol frequencies. For the
;; extreme case of exercise 2.71 that means O(n) recursive calls in which
;; O(log(n)) set lookups occurs. In the end that means O(n*log(n)) for the
;; worst case.
