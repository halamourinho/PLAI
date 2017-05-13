#lang plai-typed


(define (read-number/suspend [prompt : string] rest)
    (let ([g (new-label)])
        (begin
            (hash-set! table g rest)
            (display prompt)
            (display " To enter it, use the action field label ")
            (display g))))

(define new-label
    (let ([n (box 0)])
        (lambda () (begin
            (set-box! n (add1 (unbox n)))
            (unbox n)))))

(define table (make-hash empty))

(define (resume [g : number] [n : number])
    ((some-v (hash-ref table g)) n))

(define-syntax (cps e)
    (syntax-case e (with rec lam cnd seq set quote display read-number)
        [(_ (with (v e) b))
            #'(cps ((lam (v) b) e))]
        [(_ (rec (v f) b))
            #'(cps (with (v (lam (arg) (error 'dummy "nothing")))
                    (seq
                        (set v f)
                        b)))]
        [(_ (lam (a) b))
            (identifier? #'a)
            #'(lambda (k)
                (k (lambda (a dyn-k)
                    ((cps b) dyn-k))))]
        [(_ (cnd tst thn els))
            #'(lambda (k)
                ((cps tst) (lambda (tstv)
                            (if tstv
                                ((cps thn) k)
                                ((cps els) k)))))]
        [(_ (display output))
            #'(lambda (k)
                ((cps output) (lambda (ov)
                                (k (display ov)))))]
        [(_ (read-number prompt))
            #'(lambda (k)
                ((cps prompt) (lambda (pv)
                                (read-number/suspend pv k))))]
        [(_ (seq e1 e2))
            #'(lambda (k)
                ((cps e1) (lambda (_)
                    ((cps e2) k))))]
        [(_ (set v e))
            #'(lambda (k)
                ((cps e) (lambda (ev)
                    (k (set! v ev)))))]
        [(_ 'e)
            #'(lambda (k) (k 'e))]
        [(_ (f a))
            #'(lambda (k)
                ((cps f) (lambda (fv)
                            ((cps a) (lambda (av)
                                        (fv av k))))))]
        [(_ (f a b))
            #'(lambda (k)
                ((cps a) (lambda (av)
                            ((cps b) (lambda (bv)
                                        (k (f av bv)))))))]
        [(_ atomic)
            #'(lambda (k) (k atomic))]))

(define (run c) (c identity))

(cps (lam (x) 5))
(test (run (cps 3)) 3)
;(test (run (cps ((lam () 5) ))) 5)
(test (run (cps ((lam (x) (* x x)) 5))) 25)
(test (run (cps (+ 5 ((lam (x) (* x x)) 5)))) 30)


(lambda (k)
                ((cps (lam (x) (* x x))) (lambda (fv)
                            ((cps 5) (lambda (av)
                                        (fv av k))))))