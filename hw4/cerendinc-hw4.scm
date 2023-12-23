;- Procedure: main-procedure
;- Input : Takes only one parameter named tripleList
;- Output : Returns list of triples according to the scenario
; described in section 3.
; Returns an error if the given parameter is not a tripleList.
(define main-procedure
    (lambda (tripleList)
        (if (or (null? tripleList) (not (list? tripleList)))
            (error "ERROR305: the input should be a list full of triples")
            (if (check-triple? tripleList)
                (sort-area (filter-pythagorean (filter-triangle
                (sort-all-triples tripleList))))
                (error "ERROR305: the input should be a list full of triples")
            )
        )
    )
)


(define check-triple?
    (lambda (tripleList)
        (if (null? tripleList)
            #t
            (if (check-length? (car tripleList) 3)
                (if (check-sides? (car tripleList))
                    (check-triple? (cdr tripleList))
                    #f
                )
                #f
            )
        )
    )
)

(define check-length?
    (lambda (inTriple count)
        (if (null? inTriple)
            (if (zero? count)
                #t
                #f
            )
            (check-length? (cdr inTriple) (- count 1))
        )
    )
)

(define check-sides?
    (lambda (inTriple)
        (if (null? inTriple)
            #t
            (if (integer? (car inTriple))
                (if (<= (car inTriple) 0)
                    #f
                    (check-sides? (cdr inTriple))
                )
                #f
            )
        )
    )
)

(define sort-all-triples
    (lambda (tripleList)
        (if (null? tripleList)
            '()
            (cons (sort-triple (car tripleList)) (sort-all-triples (cdr tripleList)))
        )
    )
)

(define sort-triple
    (lambda (inTriple)
        (if (> (car inTriple) (cadr inTriple))
            (if (> (cadr inTriple) (caddr inTriple))
                (list (caddr inTriple) (cadr inTriple) (car inTriple))
                (if (> (car inTriple) (caddr inTriple))
                    (list (cadr inTriple) (caddr inTriple) (car inTriple))
                    (list (cadr inTriple) (car inTriple) (caddr inTriple))
                )
            )
            (if (> (car inTriple) (caddr inTriple))
                (if (> (cadr inTriple) (caddr inTriple))
                    (list (caddr inTriple) (car inTriple) (cadr inTriple))
                    (list (car inTriple) (caddr inTriple) (cadr inTriple))
                )
                (if (> (cadr inTriple) (caddr inTriple))
                    (list (car inTriple) (caddr inTriple) (cadr inTriple))
                    (list (car inTriple) (cadr inTriple) (caddr inTriple))
                )
                
            )
        )
    )
)

(define filter-triangle
    (lambda (tripleList)
        (if (null? tripleList)
            '()
            (if (triangle? (car tripleList))
                (cons (car tripleList) (filter-triangle (cdr tripleList)))
                (filter-triangle (cdr tripleList))
            )
        )
    )
)

(define triangle?
    (lambda (triple)
        (if (> (+ (car triple) (cadr triple)) (caddr triple))
            #t
            #f
        )
    )
)

(define filter-pythagorean
    (lambda (tripleList)
        (if (null? tripleList)
            '()
            (if (pythagorean-triangle? (car tripleList))
                (cons (car tripleList) (filter-pythagorean (cdr tripleList)))
                (filter-pythagorean (cdr tripleList))
            )
        )
    )
)


(define pythagorean-triangle?
    (lambda (triple)
        (if (eq? (+ (* (car triple) (car triple)) (* (cadr triple) (cadr triple))) (* (caddr triple) (caddr triple)))
            #t
            #f
        )
    )
)



(define sort-area
    (lambda (tripleList)
        (define (insert new sList)
            (if (null? sList)
                (list new)
                (if (< (get-area new) (get-area (car sList)))
                    (cons new sList)
                    (cons (car sList) (insert new (cdr sList)))
                )
            )
        )
        (define (sort-helper uList sList)
            (if (null? uList)
                sList
                (sort-helper (cdr uList) (insert (car uList) sList))
            )
        )
        (sort-helper (cdr tripleList) (list (car tripleList)))
    )
)

(define get-area
    (lambda (triple)
        (/ (* (car triple) (cadr triple)) 2)
    )
)



