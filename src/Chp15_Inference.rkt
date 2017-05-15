#lang plai-typed

;Type Inference
(define-type ExprC
    [numC (n : number)]
    [idC (s : symbol)]
    [appC (fun : ExprC) (arg : ExprC)]
    [plusC (l : ExprC) (r : ExprC)]
    [multC (l : ExprC) (r : ExprC)]
    [lamC (arg : symbol) (body : ExprC)])

(define-type Constraints
    [eqCon (lhs : Term) (rhs : Term)])

(define-type Term
    [tExp (e : ExprC)]
    [tVar (s : symbol)]
    [tNum]
    [tArrow (dom : Term) (rng : Term)])

(define (append3 l1 l2 l3) (append l1 (append l2 l3)))

(define (cg [e : ExprC]) : (listof Constraints) ;Generate constraints
    (type-case ExprC e
    [numC (_) (list (eqCon (tExp e) (tNum)))]
    [idC (s) (list (eqCon (tExp e) (tVar s)))]
    [plusC (l r) (append3 (cg l)
                        (cg r)
                        (list (eqCon (tExp l) (tNum))
                            (eqCon (tExp r) (tNum))
                            (eqCon (tExp e) (tNum))))]
    [multC (l r) (append3 (cg l)
                        (cg r)
                        (list (eqCon (tExp l) (tNum))
                            (eqCon (tExp r) (tNum))
                            (eqCon (tExp e) (tNum))))]
    [lamC (a b) (append (cg b)
                    (list (eqCon (tExp e) (tArrow (tVar a) (tExp b)))))]
    [appC (f a) (append3 (cg f)
                        (cg a)
                        (list (eqCon (tExp f) (tArrow (tExp a) (tExp e)))))]))

(define-type-alias Subst (listof Substitution))
(define-type Substitution
    [sub [var : Term] [is : Term]])

(define (unify [cs : (listof Constraints)]) : Subst
    (unify/Θ cs empty))

(define (unify/Θ [cs : (listof Constraints)] [Θ : Subst]) : Subst
    (cond
        [(empty? cs) Θ]
        [(cons? cs)
            (let ([l (eqCon-lhs (first cs))]
                [r (eqCon-rhs (first cs))])
                (type-case Term l
                    [tVar (s) (type-case (optionof Term) (lookup l Θ)
                                [some (bound)
                                    (unify/Θ (cons (eqCon bound r) (rest cs)) Θ)]
                                [none ()
                                    (unify/Θ (rest cs) (extend+replace l r Θ))])]    
                    [tExp (s) (type-case (optionof Term) (lookup l Θ)
                                [some (bound)
                                    (unify/Θ (cons (eqCon bound r) (rest cs)) Θ)]
                                [none ()
                                    (unify/Θ (rest cs) (extend+replace l r Θ))])] 
                    [tNum () (type-case Term r
                                [tNum () (unify/Θ (rest cs) Θ)]
                                [else (error 'unify "number and something else")])]
                    [tArrow (d r) (type-case Term r
                                    [tArrow (d2 r2)
                                        (unify/Θ (cons (eqCon d d2)
                                                    (cons (eqCon r r2)
                                                        cs))
                                            Θ)]
                                    [else (error 'unify "arrow and something else")])]))]))

(define (lookup [t : Term] [ls : Subst]) : (optionof Term)
    (cond
        [(empty? ls) (none)]
        [else (type-case Substitution (first ls)
            [sub (var is) (if (eq? var t)
                            (some is)
                            (lookup t (rest ls)))])]))

(define (occurs? [l : Term] [r : Term]) : boolean
    (type-case Term r
        [tArrow (dom rng) (or (occurs? l dom) (occurs? l rng))]
        [else (eq? l r)]))

(define (replace-all [s : Subst] [l : Term] [r : Term]) : Subst
    (cond
        [(empty? s) empty]
        [(cons? s)
            (let ([var (sub-var (first s))]
                [is (replace (sub-is (first s)) l r)])
                (cons (sub var is) (replace-all (rest s) l r)))]))
[define (replace [t : Term] [l : Term] [r : Term]) : Term
    (type-case Term t
        [tArrow (dom rng) (tArrow (replace dom l r) (replace rng l r))]
        [else (if (eq? t l) r t)])]

(define (extend+replace [l : Term] [r : Term] [s : Subst]) : Subst  
    (cond
      [(occurs? l r) (error 'extend+replace "cycle in substitution")]
      [else (cons (sub l r) (replace-all s l r))]))