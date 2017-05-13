#lang racket


(define-syntax my-let-1
    (syntax-rules ()
        [(my-let-1 (var val) body)
        ((lambda (var) body) val)]))

(define-syntax my-let-2
    (syntax-rules ()
        [(my-let-2 ([var val] ...) body)
        ((lambda (var ...) body) val ...)]))

(define-syntax (my-let-3 x)
    (syntax-case x ()
        [(my-let-3 (var val) body)
        (identifier? #'var)
        #'((lambda (var) body) val)]))

(define-syntax (my-or-3 x)
    (syntax-case x ()
        [(my-or-3)
            #'#f]
        [(my-or-3 e)
            #'e]
        [(my-or-3 e0 e1 ...)
            #'(if e0
                e0
                (my-or-3 e1 ...))]))

(define-syntax (my-or-4 x)
    (syntax-case x ()
        [(my-or-4)
            #'#f]
        [(my-or-4 e)
            #'e]
        [(my-or-4 e0 e1 ...)
            #'(let ([v e0])
                (if v
                    v
                    (my-or-4 e1 ...)))]))
;This is not a Hygiene Macro. Consider (let ([v #t]) (my-or-4 #f v))

(define-syntax (my-or-5 x)
    (syntax-case x ()
        [(my-or-5)
            #'#f]
        [(my-or-5 e)
            #'e]
        [(my-or-5 e0 e1 ...)
            #'(if e0
                #t
                (my-or-3 e1 ...))]))

(define (msg obj mtd arg)
    ((obj mtd) arg))

; (define os-1
;     (object/self-1
;         [first (x) (msg self 'second (+ x 1))]
;         [second (x) (+ x 1)]))

; (define-syntax object/self-1
;     (syntax-rules ()
;         [(object/self-1 [mtd-name (var) val] ...)
;             (let ([self (lambda (msg-name)
;                     (lambda (v) (error 'object "nothing here")))])
;                 (begin
;                     (set! self
;                         (lambda (msg)
;                             (case msg
;                                 [(mtd-name) (lambda (var) val)]
;                                 ...)))
;                 self))]))

(define os-2
    (object/self-2 self
        [first (x) (msg self 'second (+ x 1))]
        [second (x) (+ x 1)]))

(define-syntax object/self-2
    (syntax-rules ()
        [(object/self-2 self [mtd-name (var) val] ...)
            (let ([self (lambda (msg-name)
                    (lambda (v) (error 'object "nothing here")))])
                (begin
                    (set! self
                        (lambda (msg)
                            (case msg
                                [(mtd-name) (lambda (var) val)]
                                ...)))
                self))]))

(define os-3
    (object/self-3
        [first (x) (msg self 'second (+ x 1))]
        [second (x) (+ x 1)]))

(define-syntax (object/self-3 x)
    (syntax-case x ()
        [(object [mtd-name (var) val] ...)
            (with-syntax ([self (datum->syntax x 'self)])
                #'(let ([self (lambda (msg-name)
                            (lambda (v) (error 'object "nothing here")))])
                    (begin
                        (set! self
                            (lambda (msg-name)
                                (case msg-name
                                    [(mtd-name) (lambda (var) val)]
                                    ...)))
                        self)))]))


