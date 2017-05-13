#lang plai-typed

(define-type ExprC
    [numC (n : number)]
    [idC (s : symbol)]
    [appC (fun : ExprC) (arg : ExprC)]
    [plusC (l : ExprC) (r : ExprC)]
    [multC (l : ExprC) (r : ExprC)]
    [lamC (arg : symbol) (body : ExprC)])

(define-type Value
    [numV (n : number)]
    [closV (f : (Value (Value -> Value) -> Value))])

(define-type Binding [bind (name : symbol) (val : Value)])
(define-type-alias Env (listof Binding))
(define mt-env empty)
(define extend-env cons)

(define (lookup [for : symbol] [env : Env]) : Value
    (cond
        [(empty? env) (error 'lookup "name not found")]
        [else (cond
            [(symbol=? for (bind-name (first env))) (bind-val (first env))]
            [else (lookup for (rest env))])]))
            
(define (interp/k [expr : ExprC] [env : Env] [k : (Value -> Value)]) : Value
    (type-case ExprC expr
        [numC (n) (k (numV n))]
        [idC (n) (k (lookup n env))]
        [appC (f a) (interp/k f env
                        (lambda (fv)
                            (interp/k a env
                                (lambda (av)
                                    ((closV-f fv) av k)))))]
        [plusC (l r) (interp/k l env
                        (lambda (lv)
                            (interp/k r env
                                (lambda (rv)
                                    (k (num+ lv rv))))))]
        [multC (l r) (interp/k l env
                        (lambda (lv)
                            (interp/k r env
                                (lambda (rv)
                                    (k (num* lv rv))))))]
        [lamC (a b) (k (closV (lambda (arg-val dyn-k)
                                (interp/k b
                                    (extend-env (bind a arg-val) env)
                                    dyn-k))))]))

(define (opV [op : (number number -> number)])
    (lambda ([l : Value] [r : Value])
        (cond
            [(and (numV? l) (numV? r))
                (numV (op (numV-n l) (numV-n r)))]
            [else
                (error 'num+ "one argument was not a number")])))
(define num+ (opV +))
(define num* (opV *))

(define (interp [expr : ExprC] [env : Env]) : Value
    (interp/k expr env (lambda (ans) ans)))

;Tests
(test (interp (plusC (numC 10) (appC (lamC '_ (numC 5)) (numC 10)))
        mt-env)
    (numV 15))
(test (interp (appC (lamC 'x (appC (lamC 'y (plusC (idC 'x) (idC 'y)))
                                    (numC 4)))
                    (numC 3))
            mt-env)
    (numV 7))






