#lang typed/racket
(require typed/rackunit)

;; OAZO3 

;ExprC language declaration


(define-type ExprC (U numC plusC multC subC divC appC idC ifleq0C))
(struct numC ([n : Real]) #:transparent)
(struct plusC ([l : ExprC] [r : ExprC]) #:transparent)
(struct multC ([l : ExprC] [r : ExprC]) #:transparent)
(struct subC ([l : ExprC] [r : ExprC]) #:transparent)
(struct divC ([l : ExprC] [r : ExprC]) #:transparent)
(struct appC ([funid : Symbol] [param : ExprC]) #:transparent)
(struct idC ([id : Symbol]) #:transparent)
(struct ifleq0C ([c : ExprC] [y : ExprC] [n : ExprC]) #:transparent)


;parse
 ;accepts an s-expression code
 ;returns the parsed ExprC representation of code
(define (parse [code : Sexp]) : ExprC
  (match code
    [(? real? n)   (numC n)]
    [(? symbol? s) (idC s)]
    [(list '+ l r) (plusC (parse l) (parse r))]
    [(list '* l r) (multC (parse l) (parse r))]
    [(list '- l r) (subC (parse l) (parse r))]
    [(list '/ l r) (divC (parse l) (parse r))]
    [(list 'ifleq0? c y n) (ifleq0C (parse c) (parse y) (parse n))]
    [other (error 'OAZO3-parse "syntax error in ~e" other)]))

;tests
(check-equal? (parse '{+ 1 2}) (plusC (numC 1) (numC 2)))
(check-equal? (parse '{* 1 2}) (multC (numC 1) (numC 2)))
(check-equal? (parse '{- 1 2}) (subC (numC 1) (numC 2)))
(check-equal? (parse '{/ 1 2}) (divC (numC 1) (numC 2)))
(check-equal? (parse '{ifleq0? 1 2 3}) (ifleq0C (numC 1) (numC 2) (numC 3)))
(check-equal? (parse '{ifleq0? -1 5 3}) (ifleq0C (numC -1) (numC 5) (numC 3)))
(check-equal? (parse 'id) (idC 'id))
(check-exn #rx"syntax error" (lambda () (parse '(+ 2))))


;interp
 ;accepts an ExprC a
 ;returns the evaluation of a
(define (interp [a : ExprC]) : Real 
  (match a
    [(numC n) n]
    #;([(idC s) s])
    [(plusC l r) (+ (interp l) (interp r))]
    [(multC l r) (* (interp l) (interp r))]
    [(subC l r) (- (interp l) (interp r))]
    [(divC l r) (/ (interp l) (interp r))]
    [(ifleq0C c y n) (cond [(<= (interp c) 0) (interp y)]
                           [else (interp n)])]))

;tests
(check-equal? (interp (plusC (numC 3) (numC 4))) 7)
(check-equal? (interp (multC (numC 3) (numC 4))) 12)
(check-equal? (interp (multC (plusC (numC 4) (numC 4)) (numC 4))) 32)




;top-interp
 ;accepts s-expression s
 ;returns the interpretation of the parse of s
(define (top-interp [s : Sexp]) : Real
  (interp (parse s)))

;tests
(check-equal? (top-interp '(* 2 3)) 6)
(check-equal? (top-interp '(+ 2 3)) 5)
(check-equal? (top-interp '(- 2 3)) -1)
(check-equal? (top-interp '(/ 6 3)) 2)
(check-equal? (top-interp '(ifleq0? 6 3 2)) 2)
(check-equal? (top-interp '(ifleq0? -1 3 2)) 3)
(check-exn #rx"syntax error" (lambda () (top-interp '(+ 2))))
(check-exn #rx"syntax error" (lambda () (top-interp '())))


