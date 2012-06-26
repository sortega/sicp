(ns sicp.chapter2-2
  (:use [sicp.chapter1-2 :only [abs prime? square]])
  (:use [useful.seq :only [foldr]])
  (:use [quil.core :exclude [abs]]))

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


;; Support methods
;; ===============

(declare beside below flip-horiz flip-vert up-split)

(defn flipped-pairs [painter]
  (let [painter2 (beside painter (flip-vert painter))]
    (below painter2 painter2)))

(defn right-split [painter n]
  (if (zero? n)
    painter
    (let [smaller (right-split painter (dec n))]
      (beside painter (below smaller smaller)))))

(defn corner-split [painter n]
  (if (zero? n)
    painter
    (let [up           (up-split     painter (dec n))
          right        (right-split  painter (dec n))
          top-left     (beside up up)
          bottom-right (below right right)
          corner       (corner-split painter (dec n))]
        (beside (below painter top-left)
                (below bottom-right corner)))))

(defn square-limit [painter n]
  (let [quarter (corner-split painter n)
        half (beside (flip-horiz quarter) quarter)]
    (below (flip-vert half) half)))


;; Exercise 2.44
;; =============

(defn up-split [painter n]
  (if (zero? n)
    painter
    (let [smaller (up-split painter (dec n))]
      (beside painter (beside smaller smaller)))))


;; HOF samples
;; ===========

(defn square-of-four [tl tr bl br]
  (fn [painter]
    (let [top (beside (tl painter) (tr painter))
          bottom (beside (bl painter) (br painter))]
      (below bottom top))))

(defn flipped-pairs [painter]
  (let [combine4 (square-of-four identity flip-vert
                                 identity flip-vert)]
    (combine4 painter)))

(defn square-limit [painter n]
  (let [combine4 (square-of-four flip-horiz identity
                                 identity   flip-vert)]
    (combine4 (corner-split painter n))))


;; Exercise 2.45
;; =============

(defn split [outer-comb inner-comb]
  (fn split-fn [painter n]
    (if (zero? n)
      painter
      (let [smaller (split-fn painter (dec n))]
        (outer-comb painter (inner-comb smaller smaller))))))

(def right-split (split beside below))
(def up-split    (split below beside))

;;

(declare add-vect
         edge1-frame
         edge2-frame
         make-vect
         origin-frame
         scale-vect 
         xcor-vect 
         ycor-vect)

(defn frame-coord-map [frame]
  (fn [v]
    (add-vect
      (origin-frame frame)
      (add-vect (scale-vect (xcor-vect v)
                            (edge1-frame frame))
                (scale-vect (ycor-vect v)
                            (edge2-frame frame))))))


;; Exercise 2.46
;; =============

(defn make-vect [x y]
  {:x x
   :y y})
(defn make-vects [& coords]
  (map #(apply make-vect %) (partition 2 coords)))
(def xcor-vect :x)
(def ycor-vect :y)

(defn add-vect [v w]
  (make-vect
    (+ (xcor-vect v) (xcor-vect w))
    (+ (ycor-vect v) (ycor-vect w))))

(defn sub-vect [v w]
  (make-vect
    (- (xcor-vect v) (xcor-vect w))
    (- (ycor-vect v) (ycor-vect w))))

(defn scale-vect [k v]
  (make-vect
    (* k (xcor-vect v))
    (* k (ycor-vect v))))


;; Exercise 2.47
;; =============

(defn make-frame [origin edge1 edge2]
  (list origin edge1 edge2))

(def origin-frame first)
(def edge1-frame second)
(defn edge2-frame [[_ _ third]] third)

;; Painters
;; ========

(declare start-segment end-segment)

(defn segments->painter [segment-list]
  (fn [frame]
    (let [cmap (frame-coord-map frame)]
      (stroke 0)
      (stroke-weight 1)
      (doseq [segment segment-list]
        (let [start (cmap (start-segment segment))
              end   (cmap (end-segment segment))]
          (line (xcor-vect start)
                (ycor-vect start)
                (xcor-vect end)
                (ycor-vect end)))))))


;; Exercise 2.48
;; =============

(defn make-segment [start end]
  {:start start
   :end end})
(def start-segment :start)
(def end-segment :end)


;; Exercise 2.49
;; =============

(defn make-polyline [points]
  (map #(apply make-segment %)
       (partition 2 1 points)))

(defn make-polygon [points]
  (map #(apply make-segment %)
       (partition 2 1 points points)))

(def outline-frame
  (segments->painter 
    (make-polygon (make-vects 0 0, 0 1, 1 1, 1 0))))

(def cross (segments->painter 
             [(make-segment (make-vect 0 0) (make-vect 1 1))
              (make-segment (make-vect 0 1) (make-vect 0 1))]))

(def diamond
  (segments->painter
   (make-polygon (make-vects 1 0.5, 0.5 1, 0 0.5, 0.5 0))))

(def wave 
  (segments->painter
    (concat
      (make-polygon (make-vects 4/8 0, 5/8 1/8, 4/8 3/8, 3/8 1/8))    ; head
      (make-polygon (make-vects 6/8 3/8, 8/8 6/8, 6/8 4/8, 5/8 4/8,   ; arm 1
                                6/8 8/8, 4/8 6/8, 2/8 8/8,            ; legs
                                3/8 4/8, 2/8 4/8, 0 2/8, 2/8 3/8))))) ; arm 2

;; Transforming and combining painters
;; ===================================

(defn transform-painter [painter origin corner1 corner2]
  (fn [frame]
    (let [m          (frame-coord-map frame)
          new-origin (m origin)]
      (painter
        (make-frame new-origin
                    (sub-vect (m corner1) new-origin)
                    (sub-vect (m corner2) new-origin))))))

(defn flip-vert [painter]
  (transform-painter painter
                     (make-vect 0 1)   ; new `origin'
                     (make-vect 1 1)   ; new end of `edge1'
                     (make-vect 0 0))) ; new end of `edge2'

(defn shrink-to-upper-right [painter]
  (transform-painter painter
                     (make-vect 0.5 0.5)
                     (make-vect 1.0 0.5)
                     (make-vect 0.5 1.0)))

(defn rotate90 [painter]
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

(defn beside [painter1 painter2]
  (let [split-point (make-vect 0.5 0.0)
        paint-left  (transform-painter painter1
                                       (make-vect 0.0 0.0)
                                       split-point
                                       (make-vect 0.0 1.0))
        paint-right (transform-painter painter2
                                       split-point
                                       (make-vect 1.0 0.0)
                                       (make-vect 0.5 1.0))]
      (fn [frame]
        (paint-left frame)
        (paint-right frame))))


;; Exercise 2.50
;; =============

(defn flip-horiz [painter]
  (transform-painter painter
                     (make-vect 1 0)   ; new `origin'
                     (make-vect 0 0)   ; new end of `edge1'
                     (make-vect 1 1))) ; new end of `edge2'


;; Exercise 2.51
;; =============

(defn below [painter1 painter2]
  (let [split-point  (make-vect 0.0 0.5)
        paint-top    (transform-painter painter1
                                        (make-vect 0.0 0.0)
                                        (make-vect 1.0 0.0)
                                        split-point)
        paint-bottom (transform-painter painter2
                                        split-point
                                        (make-vect 1.0 0.5)
                                        (make-vect 0.0 1.0))]
      (fn [frame]
        (paint-top frame)
        (paint-bottom frame))))



;; Exercise 2.52
;; =============

; a) Now with an "smile"
(def wave 
  (segments->painter
    (concat
      (make-polygon  (make-vects 4/8 0, 5/8 1/8, 4/8 3/8, 3/8 1/8))    ; head
      (make-polyline (make-vects 7/16 3/16, 4/8 2/8, 9/16 3/16))       ; smile
      (make-polygon  (make-vects 6/8 3/8, 8/8 6/8, 6/8 4/8, 5/8 4/8,   ; arm 1
                                 6/8 8/8, 4/8 6/8, 2/8 8/8,            ; legs
                                 3/8 4/8, 2/8 4/8, 0 2/8, 2/8 3/8))))) ; arm 2

; b) One copy of up-split
(defn corner-split [painter n]
  (if (zero? n)
    painter
    (let [up           (up-split     painter (dec n))
          right        (right-split  painter (dec n))
          top-left     (beside up up)
          bottom-right (below right right)
          corner       (corner-split painter (dec n))]
        (beside (below painter top-left)
                (below bottom-right corner)))))

; c) different arrangement
(defn square-limit [painter n]
  (let [quarter (corner-split painter n)
        half (beside quarter (flip-horiz quarter))]
    (below (flip-vert half) half)))


;; GUI scafolding
;; ==============

(def painter (atom wave))

(defn paint []
  (quil.applet/applet
    :title "SICP painter output"
    :setup (fn []
             (smooth)
             (frame-rate 10))
    :draw  (fn []
             (background 200)
             (@painter (make-frame (make-vect 0 0)
                                   (make-vect (width) 0)
                                   (make-vect 0 (height)))))
    :size [323 200]))

(defn set-painter [paint]
  (reset! painter paint))
