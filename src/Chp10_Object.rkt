#lang plai-typed

(define-type ExprC
    [numC (n : number)]
    [idC (s : symbol)]
    [appC (fun : ExprC) (arg : ExprC)]
    [plusC (l : ExprC) (r : ExprC)]
    [multC (l : ExprC) (r : ExprC)]
    [lamC (arg : symbol) (body : ExprC)]
    [setC (var : symbol) (arg : ExprC)]
    [seqC (b1 : ExprC) (b2 : ExprC)]
    [objC (ns : (listof symbol)) (es : (listof ExprC))]
    [msgC (o : ExprC) (n : symbol)]
    [msgS (o : ExprC) (n : symbol) (a : ExprC)]
    [letC (var : symbol) (val : ExprC) (e : ExprC)])

(define new-loc
    (let ([n (box 0)])
        (lambda () (begin
            (set-box! n (add1 (unbox n)))
            (unbox n)))))

(define-type Value
    [numV (n : number)]
    [closV (arg : symbol) (body : ExprC) (env : Env)]
    [objV (ns : (listof symbol)) (vs : (listof Value))])

(define-type-alias Location number)
(define-type Binding
    [bind (name : symbol) (val : Location)])
(define-type-alias Env (listof Binding))
(define mt-env empty)
(define extend-env cons)
(define-type Storage
    [cell (location : Location) (val : Value)])
(define-type-alias Store (listof Storage))
(define mt-store empty)
(define override-store cons)

(define (lookup [for : symbol] [env : Env]) : Location
    (cond
        [(empty? env) (error 'lookup "name not found")]
        [else (cond
            [(symbol=? for (bind-name (first env))) (bind-val (first env))]
            [else (lookup for (rest env))])]))

(define (fetch [loc : Location] [sto : Store]) : Value
    (cond
        [(empty? sto) (error 'lookup "name not found")]
        [else (cond
            [(= loc (cell-location (first sto))) (cell-val (first sto))]
            [else (fetch loc (rest sto))])]))

(define (lookup-msg [for : symbol] [o : Value]) : Value
    (type-case Value o
        [objV (ns vs) (cond
                        [(empty? ns) (error 'lookup-msg "name not found")]
                        [else (cond
                                [(symbol=? for (first ns)) (first vs)]
                                [else (lookup-msg for (objV (rest ns) (rest vs)))])])]
        [else (error 'lookip-msg "not a object")]))

(define-type Result
    [v*s (v : Value) (s : Store)])


(define (interp [expr : ExprC] [env : Env] [sto : Store]) : Result
    (type-case ExprC expr
        [numC (n) (v*s (numV n) sto)]
        [idC (n) (v*s (fetch (lookup n env) sto) sto)]
        [appC (f a) (type-case Result (interp f env sto)
                    [v*s (v-f s-f)
                        (type-case Result (interp a env s-f)
                        [v*s (v-a s-a)
                            (let ([where (new-loc)])
                                (interp (closV-body v-f)
                                    (extend-env (bind (closV-arg v-f) where) (closV-env v-f))
                                    (override-store (cell where v-a) s-a)))])])]
        [plusC (l r) (type-case Result (interp l env sto)
                        [v*s (v-l s-l)
                            (type-case Result (interp r env s-l)
                                [v*s (v-r s-r)
                                    (v*s (num+ v-l v-r) s-r)])])]
        [multC (l r) (type-case Result (interp l env sto)
                        [v*s (v-l s-l)
                            (type-case Result (interp r env s-l)
                                [v*s (v-r s-r)
                                    (v*s (num* v-l v-r) s-r)])])]
        [lamC (a b) (v*s (closV a b env) sto)]
        [setC (var val) (type-case Result (interp val env sto)
                        [v*s (v-val s-val)
                            (let ([where (lookup var env)])
                                (v*s v-val
                                    (override-store (cell where v-val)
                                        s-val)))])]
        [seqC (b1 b2) (type-case Result (interp b1 env sto)
                        [v*s (v-b1 s-b1)
                            (interp b2 env s-b1)])]
        [objC (ns es) (v*s (objV ns (map (lambda (e)
                                    (v*s-v (interp e env sto))) ;suppose there is no set! in es
                                es)) sto)]
        [msgC (o n) (v*s (lookup-msg n (v*s-v (interp o env sto))) sto)]
        [msgS (o n a) (interp (appC (msgC o n) a) env sto)]
        [letC (var val e) (interp (appC (lamC var e) val) env sto)]))

(define (opV [op : (number number -> number)])
    (lambda ([l : Value] [r : Value])
        (cond
            [(and (numV? l) (numV? r))
                (numV (op (numV-n l) (numV-n r)))]
            [else
                (error 'num+ "one argument was not a number")])))
(define num+ (opV +))
(define num* (opV *))


;Tests
(test (v*s-v (interp (plusC (numC 10) (appC (lamC '_ (numC 5)) (numC 10)))
        mt-env mt-store))
    (numV 15))
(test (v*s-v (interp (appC (lamC 'x (appC (lamC 'y (plusC (idC 'x) (idC 'y)))
                                    (numC 4)))
                    (numC 3))
            mt-env mt-store))
    (numV 7))

(test (v*s-v (interp (msgS (objC (list 'add1 'sub1)
                                (list (lamC 'x (plusC (idC 'x) (numC 1)))
                                        (lamC 'x (plusC (idC 'x) (numC -1))))) 
                            'add1 (numC 3))
            mt-env mt-store))
    (numV 4))

(test (v*s-v (interp (letC 'o (objC (list 'add1 'sub1)
                                (list (lamC 'x (plusC (idC 'x) (numC 1)))
                                        (lamC 'x (plusC (idC 'x) (numC -1))))) 
                            (msgS (idC 'o) 'add1 (numC 3)))
            mt-env mt-store))
    (numV 4))


