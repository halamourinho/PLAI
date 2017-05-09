#lang plai-typed

;
(define-type ArithC
    [numC (n : number)]
    [plusC (l : ArithC) (r : ArithC)]
    [multC (l : ArithC) (r : ArithC)])

(define (interp [a : ArithC]) : number
    (type-case ArithC a
        [numC (n) n]
        [plusC (l r) (+ (interp l) (interp r))]
        [multC (l r) (* (interp l) (interp r))]))

;
(define-type ArithS
    [numS (n : number)]
    [plusS (l : ArithS) (r : ArithS)]
    [bminusS (l : ArithS) (r : ArithS)]
    [multS (l : ArithS) (r : ArithS)]
    [uminusS (e : ArithS)])

(define (desugar [as : ArithS]) : ArithC
    (type-case ArithS as
        [numS (n) (numC n)]
        [plusS (l r) (plusC (desugar l) (desugar r))]
        [multS (l r) (multC (desugar l) (desugar r))]
        [bminusS (l r) (plusC (desugar l) (multC (numC -1) (desugar r)))]
        [uminusS (e) (multC (numC -1) (desugar e))]))







