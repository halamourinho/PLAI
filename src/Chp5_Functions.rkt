#lang plai-typed

(define-type ExprC
    [numC (n : number)]
    [idC (s : symbol)]
    [appC (fun : symbol) (arg : ExprC)]
    [plusC (l : ExprC) (r : ExprC)]
    [multC (l : ExprC) (r : ExprC)])

(define-type FunDefC [fdC (name : symbol) (arg : symbol) (body : ExprC)])

(define (subst [what : ExprC] [for : symbol] [in : ExprC]) : ExprC
    (type-case ExprC in
        [numC (n) in]
        [idC (s) (cond
            [(symbol=? s for) what]
            [else in])]
        [appC (f a) (appC f (subst what for a))]
        [plusC (l r) (plusC (subst what for l) (subst what for r))]
        [multC (l r) (multC (subst what for l) (subst what for r))]))

(define (get-fundef [n : symbol] [fds : (listof FunDefC)]) : FunDefC
    (cond
        [(empty? fds) (error 'get-fundef "reference to undefined function")]
        [(cons? fds) (cond
                        [(equal? n (fdC-name (first fds))) (first fds)]
                        [else (get-fundef n (rest fds))])]))

(define (interp [e : ExprC] [fds : (listof FunDefC)]) : number
    (type-case ExprC e
        [numC (n) n]
        [idC (_) (error 'interp "shouldn't get here")]
        [appC (f a) (local ([define fd (get-fundef f fds)]) (interp (subst a (fdC-arg fd) (fdC-body fd)) fds))]
        [plusC (l r) (+ (interp l fds) (interp r fds))]
        [multC (l r) (* (interp l fds) (interp r fds))]))


;Tests
(test (interp (plusC (numC 10) (appC 'const5 (numC 10)))
    (list (fdC 'const5 '_ (numC 5))))
    15)
(test (interp (plusC (numC 10) (appC 'double (plusC (numC 1) (numC 2))))
    (list (fdC 'double 'x (plusC (idC 'x) (idC 'x)))))
    16)
(test (interp (plusC (numC 10) (appC 'quadruple (plusC (numC 1) (numC 2))))
    (list (fdC 'quadruple 'x (appC 'double (appC 'double (idC 'x))))
    (fdC 'double 'x (plusC (idC 'x) (idC 'x)))))
    22)










