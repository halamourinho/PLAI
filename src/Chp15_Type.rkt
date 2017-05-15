#lang plai-typed

(define-type TyExprC
    [numC (n : number)]
    [idC (s : symbol)]
    [appC (fun : TyExprC) (arg : TyExprC)]
    [plusC (l : TyExprC) (r : TyExprC)]
    [multC (l : TyExprC) (r : TyExprC)]
    [lamC (arg : symbol) (argT : Type) (retT : Type) (body : TyExprC)]
    [recC (f : symbol) (arg : symbol) (argT : Type) (retT : Type) (body : TyExprC) (use : TyExprC)])

(define-type-alias Env (listof number))   ;dummy
(define-type Value
    [numV (n : number)]
    [closV (arg : symbol) (body : TyExprC) (env : Env)])

(define-type Type
    [numT]
    [funT (arg : Type) (ret : Type)])


(define-type Binding [bind (name : symbol) (type : Type)])
(define-type-alias TyEnv (listof Binding))
(define mt-env empty)
(define extend-ty-env cons)

(define (lookup [for : symbol] [env : TyEnv]) : Type
    (cond
        [(empty? env) (error 'lookup "name not found")]
        [else (cond
            [(symbol=? for (bind-name (first env))) (bind-type (first env))]
            [else (lookup for (rest env))])]))

(define (tc [expr : TyExprC] [tenv : TyEnv]) : Type
    (type-case TyExprC expr
    [numC (n) (numT)]
    [idC (n) (lookup n tenv)]
    [plusC (l r) 
        (let ([lt (tc l tenv)]
            [rt (tc r tenv)])
            (if (and (equal? lt (numT)) (equal? rt (numT)))
                (numT)
            (error 'tc "+ not both numbers")))]
    [multC (l r) 
        (let ([lt (tc l tenv)]
            [rt (tc r tenv)])
            (if (and (equal? lt (numT)) (equal? rt (numT)))
                (numT)
            (error 'tc "* not both numbers")))]       
    [appC (f a) (let ([ft (tc f tenv)]
                        [at (tc a tenv)])
                    (cond
                        [(not (funT? ft))
                            (error 'tc "not a function")]
                        [(not (equal? (funT-arg ft) at))
                            (error 'tc "app arg mismatch")]
                        [else (funT-ret ft)]))]      
    [lamC (a argT retT b)
        (if (equal? (tc b (extend-ty-env (bind a argT) tenv)) retT)
            (funT argT retT)
            (error 'tc "lam type mismatch"))]
    [recC (f a aT rT b u)
(let ([extended-env
(extend-ty-env (bind f (funT aT rT)) tenv)])
(cond
[(not (equal? rT (tc b
(extend-ty-env
(bind a aT)
extended-env))))
(error 'tc "body return type not correct")]
[else (tc u extended-env)]))]))




