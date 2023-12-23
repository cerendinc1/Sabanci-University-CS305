

(define define-statement?
        (lambda (e)
                (and 
                        (list? e)
                        (= (length e) 3)
                        (equal? (car e) 'define)
                        (symbol? (cadr e))
                )
        )
)


(define if-expr?
        (lambda (e)
                (and
                        (list? e)
                        (= (length e) 4)
                        (equal? (car e) 'if)
                )
        )
)

(define get-symbol
        (lambda (opr-symbol env)
                (if (equal? opr-symbol '+)
                        +
                        (if (equal? opr-symbol '-)
                                -
                                (if (equal? opr-symbol '*)
                                        *
                                        (if (equal? opr-symbol '/)
                                                /
                                                (let ((dummy (display "cs305: ERROR\n\n"))) (repl env))
                                        )
                                )
                        )
                )
        )
)


(define get-val
        (lambda (var env envv)
                (if (null? env)
                        (let ((dummy (display "cs305: ERROR\n\n"))) (repl envv))
                        (if (eq? var (caar env))
                                (cdar env)
                                (get-val var (cdr env) envv)
                        )
                )
        )
)




(define cond-statement?
        (lambda (e)
                (and 
                        (list? e)
                        (> (length e) 2)
                        (equal? (car e) 'cond)
                        (cond-elem? (cdr e))

                )
        )
)


(define cond-elem?
        (lambda (e)
                (if (not (null? e))
                        (if (list? (car e))
                                (if (= 2 (length (car e)))
                                        (if (equal? (caar e) 'else)
                                                (if (null? (cdr e))
                                                        #t
                                                        #f
                                                )
                                                (cond-elem? (cdr e))
                                        )
                                        #f
                                )
                                #f
                        )
                        #f
                )
        )
)



(define let-statement?
        (lambda (e)
                (if (list? e)
                        (if (= (length e) 3)
                                (if (equal? (car e) 'let)
                                        (if (list? (cadr e))
                                                (bind-elem? (cadr e))
                                                #f
                                        )
                                        #f
                                )
                                #f
                        )
                        #f 
                )
        )
)


(define bind-elem?
        (lambda (e)
                (if (list? e)
                        (if (null? e)
                                #t
                                (if (list? (car e))
                                        (if (= 2 (length (car e)))
                                                (if (symbol? (caar e))
                                                        (bind-elem? (cdr e))
                                                        #f
                                                )
                                                #f
                                        )
                                        #f
                                )
                        )
                        #f
                )
        )
)

(define letstar-statement?
        (lambda (e)
                (if (list? e)
                        (if (= (length e) 3)
                                (if (equal? (car e) 'let*)
                                        (if (list? (cadr e))
                                                (bind-elem? (cadr e))
                                                #f
                                        )
                                        #f
                                )
                                #f
                        )
                        #f
                )
        )
)



(define no-bind
        (lambda (var envvv)
                (if (null? envvv)
                        '()
                        (if (equal? (caar envvv) var)
                                (cdr envvv)
                                (cons (car envvv) (no-bind var (cdr envvv)))
                        )
                )
        )
)


(define re-env
        (lambda (var val envvv)
                (cons (cons var val) (no-bind var envvv))
        )
)

(define cs305-interpret
        (lambda (e env)
                (cond
                        ((symbol? e) (get-val e env env))
                        ((number? e) e)
                        ((not (list? e)) (let ((dummy (display "cs305: ERROR\n\n"))) (repl env)))
                        ((if-expr? e)
                                (if (equal? (cs305-interpret (cadr e) env) 0)
                                        (cs305-interpret (cadddr e) env)
                                        (cs305-interpret (caddr e) env)
                                )
                        )
                        ((cond-statement? e)
                                (if (= 2 (length (cdr e)))
                                        (if (equal? (cs305-interpret (caadr e) env) 0)
                                                (cs305-interpret (car (cdaddr e)) env)
                                                (cs305-interpret (cadadr e) env)
                                        )
                                        (if (equal? (cs305-interpret (caadr e) env) 0)
                                                (cs305-interpret (cons (car e) (cddr e)) env)
                                                (cs305-interpret (cadadr e) env)
                                        )
                                )
                        )
                        ((let-statement? e)
                                (let*
                                        (
                                                (var-type (map cadr (cadr e)))
                                                (var-calc (map (lambda (expr) 
                                                        (cs305-interpret expr env)) var-type))
                                                (var-left  (map car (cadr e)))
                                                (env-letst   (map cons var-left var-calc))
                                                (env-letst (append env-letst env))
                                        )
                                        (cs305-interpret (caddr e) env-letst)
                                )
                        )
                        ((letstar-statement? e)
                                (if (< (length (cadr e)) 2)
                                        (cs305-interpret (cons 'let (cdr e)) env)
                                        (let*
                                                (
                                                        (strt (cons 'let (cons (cons (caadr e) '()) '())))
                                                        (create-expr (cons 'let* (cons (cdadr e) '())))
                                                        (create-expr (append create-expr (cddr e)))
                                                        (create-letstar (append strt (cons create-expr '())))
                                                )
                                                (cs305-interpret create-letstar env)
                                        )
                                )
                        )
                        (else 
                                (let
                                        (
                                                (new-symbol (get-symbol (car e) env))
                                                (neww-symbol (map cs305-interpret (cdr e) (make-list (length (cdr e)) env)))
                                        )
                                        (apply new-symbol neww-symbol)
                                )
                        )
                )
        )
)

(define repl
        (lambda (env)
                (let*
                        (
                                (dummy1 (display "cs305> "))
                                (read-expr (read))
                                (create-env (if (define-statement? read-expr)
                                        (re-env (cadr read-expr) (cs305-interpret (caddr read-expr) env) env)
                                        env))
                                (val (if (define-statement? read-expr)
                                        (cadr read-expr) 
                                        (cs305-interpret read-expr env)))
                                (dummy2 (display "cs305: "))
                                (dummy3 (display val))
                                (dummy4 (newline))
                                (dummy5 (newline))
                        )
                        (repl create-env)
                )
        )
)




(define cs305
        (lambda () (repl '()))
)





