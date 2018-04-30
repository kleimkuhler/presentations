#lang typed/racket

;; Slide: 3
(define-type Cplx (U Number (cons Number Number)))

(: creal (-> Cplx Number))
(define (creal x)
  (cond [(number? x) x]
        [else (car x)]))

;; Slide: 4
;(λ ([x : (U Number Boolean)])
;  (if (number? x) (= x 1) (not x)))

;; Slide: 8
(: .map (All (a b) (-> (-> a b) (Listof a) (Listof b))))
(define (.map f l)
  (if (null? l)
      l
      (cons (f (car l)) (.map f (cdr l)))))

;; S-expression tree
(define-type STree (U Number (cons STree STree)))

(: stree-sum (-> STree Number))
(define (stree-sum s)
  (cond [(number? s) s]
        [else (+ (stree-sum (car s))
                 (stree-sum (cdr s)))]))

;; Type inference
(: m (-> Number Number))
(define (m z)
  (let* ([x z]
         [y (* x x)])
    (- y 1)))

;(.map (λ ([x: Number]) (+ x 1)) '(1 2 3))

;; Slide: 9
;; Structure system
(define-type Tree (U leaf node))
(struct leaf ([val : Number]))
(struct node ([left : Tree] [right : Tree]))

(: tree-sum (-> Tree Number))
(define (tree-sum t)
  (cond [(leaf? t) (leaf-val t)]
        [else (+ (tree-sum (node-left t))
                 (tree-sum (node-right t)))]))

;; filter
(: the-numbers (Listof Number))
(define the-numbers
  (let ([lst (list 'a 1 'b 2 'c 3)])
    (map add1 (filter number? lst))))

;; Guide
;; Polymorphic Data Structures
;; Maybe/Option
(struct None ())
(struct (a) Some ([v : a]))

(define-type (Opt a) (U None (Some a)))

(: find (-> Number (Listof Number) (Opt Number)))
(define (find v l)
  (cond [(null? l) (None)]
        [(= v (car l)) (Some v)]
        [else (find v (cdr l))]))

;; Polymorphic Functions
(: list-length (All (A) (-> (Listof A) Integer)))
(define (list-length l)
  (if (null? l)
      0
      (add1 (list-length (cdr l)))))
