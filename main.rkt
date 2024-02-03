#lang typed/racket
(require typed/rackunit)


;Arith - language definition
(define-type ExprC (U numC binopC idC ifleq0C))
(struct numC    ([n : Real]) #:transparent)
(struct binopC  ([operation : Symbol] [l : ExprC] [r : ExprC]) #:transparent)
(struct appC    ([funid : Symbol] [param : ExprC]) #:transparent)
(struct idC     ([id : Symbol]) #:transparent)
(struct ifleq0C ([c : ExprC] [y : ExprC] [n : ExprC]) #:transparent)




;parse
 ;accepts an s-expression code
 ;returns the parsed ExprC representation of code
(define (parse [code : Sexp]) : ExprC
  (match code
    [(? real? n)   (numC n)]
    [(? symbol? s) (idC s)]
    [(list '+ l r) (binopC '+ (parse l) (parse r))]
    [(list '- l r) (binopC '- (parse l) (parse r))]
    [(list '* l r) (binopC '* (parse l) (parse r))]
    [(list '/ l r) (binopC '/ (parse l) (parse r))]
    [(list 'ifleq0? f s r) (ifleq0C (parse f) (parse s) (parse r))]
    [other (error 'OAZO3-parse "syntax error in ~e" other)]))

;tests
(check-equal? (parse 1) (numC 1))
(check-equal? (parse 'y) (idC 'y))
(check-equal? (parse '{+ 1 2}) (binopC '+ (numC 1) (numC 2)))
(check-equal? (parse '{- 1 2}) (binopC '- (numC 1) (numC 2)))
(check-equal? (parse '{* 1 2}) (binopC '* (numC 1) (numC 2)))
(check-equal? (parse '{/ 1 2}) (binopC '/ (numC 1) (numC 2)))
(check-equal? (parse '{ifleq0? -1 5 3}) (ifleq0C (numC -1) (numC 5) (numC 3)))
(check-exn #rx"syntax error" (lambda () (parse '(+ 2))))



;interp
 ;accepts an ExprC a
 ;returns the evaluation of a
(define (interp [a : ExprC]) : Real
  (match a
    [(numC n) n]
    ;[(idC s) s]
    [(binopC '+ l r) (+ (interp l) (interp r))]
    [(binopC '- l r)  (- (interp l) (interp r))]
    [(binopC '* l r) (* (interp l) (interp r))]
    [(binopC '/ l r)  (/ (interp l) (interp r))]
    [(ifleq0C c y n) (cond [(<= (interp c) 0) (interp y)]
                           [else (interp n)])]))

;tests
(check-equal? (interp (binopC '+ (numC 3) (numC 4))) 7)
(check-equal? (interp (binopC '* (numC 3) (numC 4))) 12)
(check-equal? (interp (binopC '- (numC 3) (numC 4))) -1)
(check-equal? (interp (binopC '/ (numC 3) (numC 4))) 3/4)


 
;top-interp
 ;accepts s-expression s
 ;returns the interpretation of the parse of s
(define (top-interp [s : Sexp]) : Real
  (interp (parse s)))

;tests
(check-equal? (top-interp '(+ 2 3)) 5)
(check-equal? (top-interp '(- 3 3)) 0)
(check-equal? (top-interp '(* 2 3)) 6)
(check-equal? (top-interp '(/ 4 2)) 2)
(check-equal? (top-interp '(ifleq0? 1 (+ 2 3) (- 2 3))) -1)
(check-equal? (top-interp '(ifleq0? -1 (+ 2 3) (- 2 3))) 5)
(check-exn #rx"syntax error" (lambda () (top-interp '(+ 2))))
(check-exn #rx"syntax error" (lambda () (top-interp '())))



