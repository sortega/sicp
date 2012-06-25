(ns sicp.chapter2-2
  (:use [sicp.chapter1-2 :only [abs prime? square]])
  (:use [useful.seq :only [foldr]]))

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


;; Exercise 2.24
;; =============

(defn count-leaves [x]
  (cond (not (sequential? x)) 1
        (empty? x) 0
        :else (+ (count-leaves (first x))
                 (count-leaves (rest x)))))

; (count-leaves [1 [2 [3 4]]])
; => 4
;
; as a tree 1 -> 2 -> 3 -> 4


;; Exercise 2.25
;; =============

(-> [1 3 [5 7] 9] rest rest first rest first)
(-> [[7]] first first)
(-> '(1 (2 (3 (4 (5 (6 7)))))) 
  rest first rest first rest first rest first rest first rest first)


;; Exercise 2.26
;; =============

(def x (list 1 2 3))
(def y (list 4 5 6))

;=> (concat x y)
;(1 2 3 4 5 6)

;=> (cons x y)
;((1 2 3) 4 5 6)

;=> (list x y)
;((1 2 3) (4 5 6))


;; Exercise 2.27
;; =============

(defn deep-reverse [elem]
  (if (sequential? elem) 
    (loop [aseq elem
           rev  ()]
      (if (empty? aseq)
        rev
        (recur (rest aseq)
               (cons (deep-reverse (first aseq)) rev))))
    elem))


;; Exercise 2.28
;; =============

(defn fringe [tree]
  (mapcat 
    #((if (sequential? %) fringe list) %)
    tree))


;; Exercise 2.29
;; =============

(defn make-mobile [left right]
  (list left right))

(defn make-branch [length structure]
  (list length structure))

(def left-branch first)
(def right-branch second)
(def branch-length first)
(def branch-structure second)
(def mobile? list?)

(defn total-weight [structure]
  (if (mobile? structure)
    (letfn [(branch-weight [branch]
              (total-weight (branch-structure branch)))]
      (+ (branch-weight (left-branch  structure))
         (branch-weight (right-branch structure))))
    structure))

(defn balanced? [structure]
  (if (mobile? structure)
    (let [left  (-> structure left-branch  branch-structure)
          right (-> structure right-branch branch-structure)]
      (and (balanced? left)
           (balanced? right)
           (= (total-weight left) (total-weight right))))
    true))

(comment
  ; If we replace mobile representation as follows we need no change on the
  ; total-weight or balanced? functions. ;)
  ; I've used maps insted of cons since, Clojure does not allow for improper
  ; lists (i.e. (cons 1 2)).

  (defn make-mobile [left right]
    {:left left
     :right right})
  (defn make-branch [length structure]
    {:length length
     :structure structure})

  (def left-branch :left)
  (def right-branch :right)
  (def branch-length :length)
  (def branch-structure :structure)
  (def mobile? map?))


;; Exercise 2.30
;; =============

(defn square-tree [tree]
  (if (empty? tree)
    ()
    (let [sub-tree (first tree)
          rest-tree (rest tree)]
      (cons 
         (if (sequential? sub-tree)
           (square-tree sub-tree)
           (square sub-tree))
         (square-tree rest-tree)))))

(defn square-tree [tree]
  (map (fn [sub-tree]
         (if (sequential? sub-tree)
           (square-tree sub-tree)
           (square sub-tree)))
       tree))


;; Exercise 2.31
;; =============

(defn tree-map [f tree]
  (map (fn [sub-tree]
         (if (sequential? sub-tree)
           (tree-map f sub-tree)
           (f sub-tree)))
       tree))

(def square-tree (partial tree-map square))


;; Exercise 2.32
;; =============

(defn subsets [s]
  (if (empty? s)
    '(())
    (let [elem (first s)
          subs (subsets (rest s))]
      (concat subs
              (map #(cons elem %) subs)))))


;; Exercise 2.33
;; =============

(defn sicp-map [p aseq]
  (reduce (fn [x y] (cons y x)) nil (reverse aseq)))

(defn append [seq1 seq2]
  (reduce (fn [x y] (cons y x)) seq2 (reverse seq1)))

(defn length [aseq]
  (reduce (fn [l _] (inc l)) 0 aseq))


;; Exercise 2.34
;; =============

(defn horner-eval [x coefficient-sequence]
  (reduce (fn [higher-terms this-coeff]
            (+ this-coeff
               (* higher-terms x)))
          0
          (reverse coefficient-sequence)))


;; Exercise 2.35
;; =============

(defn count-leaves [x]
  (if (sequential? x)
    (reduce + (map count-leaves x))
    1))


;; Exercise 2.36
;; =============

(defn reduce-n [op init seqs]
  (when-not (empty? (first seqs))
    (cons (reduce   op init (map first seqs))
          (reduce-n op init (map rest seqs)))))


;; Exercise 2.37
;; =============

(defn dot-product [v w]
  (reduce + (map * v w)))

(defn matrix-*-vector [m v]
  (map (partial dot-product v) m))

(defn transpose [m]
  (apply map vector m))

(defn matrix-*-matrix [m n]
  (map (partial matrix-*-vector m) n))


;; Exercise 2.38
;; =============

; (fold-right / 1 (list 1 2 3)) => 3/2
; (fold-left / 1 (list 1 2 3))  => 1/6
; (fold-right list nil (list 1 2 3)) => (1 (2 (3)))
; (fold-left list nil (list 1 2 3))  => (((1) 2) 3)
;
; To get the same result, operator should be associative as the + or *
; ones.


;; Exercise 2.39
;; =============

(defn reverse-r [aseq]
  (foldr (fn [elem accum] (concat accum [elem]))
         ()
         aseq))

(defn reverse-l [aseq]
  (reduce #(cons %2 %)
          ()
          aseq))


;; Exercise 2.40
;; =============

(defn unique-pairs [n]
  (when (> n 1)
    (concat (unique-pairs (dec n))
            (map (fn [j] [n j]) (range 1 n)))))

(defn prime-sum-pairs [n]
  (->> (unique-pairs n)
    (filter (fn [[j i]] (prime? (+ j i))))
    (map (fn [[j i]] [j i (+ j i)]))))


;; Exercise 2.41
;; =============

; List compehensions rulez!

(defn sum-triples [n s]
  (for [i (range 1 (inc n))
        j (range 1 i)
        k (range 1 j)
        :when (= s (+ i j k))]
    [k j i]))


;; Exercise 2.42
;; =============

(def empty-board [])

(defn safe? [k board]
  (let [positions (map vector (range k) board)
        k-row     (nth board k)]
    (letfn [(same-row? [[_ row]]
              (= row (nth board k)))
            (same-diag? [[col row]]
              (= (abs (- col k))
                 (abs (- row k-row))))]
      (and
        (not-any? same-row?  positions)
        (not-any? same-diag? positions)))))

(defn adjoin-position [row col board]
  (assoc board col row))

(defn queens [board-size]
  (letfn [(queen-cols [k]
            (if (zero? k)
              [empty-board]
              (->> (queen-cols (dec k))
                (mapcat (fn [rest-of-queens]
                          (map (fn [new-row]
                                 (adjoin-position new-row (dec k) rest-of-queens))
                               (range board-size))))
                (filter (partial safe? (dec k))))))]
    (queen-cols board-size)))


;; Exercise 2.43
;; =============

; The interchange is fatal since for the evaluation of (queen-cols [k]) the
; number of recursive calls is the side of the board so time is exponential.
; (So O(T^n) ?)
