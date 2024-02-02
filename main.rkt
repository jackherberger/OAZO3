#lang typed/racket
(require typed/rackunit)


;Arith language declaration

;Arith - language definition
(define-type ArithC (U numC plusC multC squareC))
(struct numC ([n : Real]) #:transparent)
(struct plusC ([l : ArithC] [r : ArithC]) #:transparent)
(struct multC ([l : ArithC] [r : ArithC]) #:transparent)
(struct squareC ([n : ArithC]) #:transparent)





;interp
 ;accepts an ArithC a
 ;returns the evaluation of a
(define (interp [a : ArithC]) : Real
  (match a
    [(numC n) n]
    [(plusC l r) (+ (interp l) (interp r))]
    [(multC l r) (* (interp l) (interp r))]
    [(squareC n) (expt (interp n) 2)]))

;tests
(check-equal? (interp (plusC (numC 3) (numC 4))) 7)
(check-equal? (interp (multC (numC 3) (numC 4))) 12)
(check-equal? (interp (multC (plusC (numC 4) (numC 4)) (numC 4))) 32)
(check-equal? (interp (squareC (numC 4))) 16)





;num-adds
 ;accepts an ArithC a
 ;prints the tree upon entry
 ;returns the number of additions in a
(define (num-adds [a : ArithC]) : Real
  (match a
    [(numC n)    (printf "(numC ~e)\n" n)
                 0]
    [(plusC l r) (printf "(plusC\n    ~e\n    ~e)\n" l r)
                 (+ 1 (num-adds l) (num-adds r))]
    [(multC l r) (printf "(multC\n    ~e\n    ~e)\n" l r)
                 (+ (num-adds l) (num-adds r))]
    [(squareC n) (printf "(squareC\n    ~e)" n)
                 (num-adds n)]))

(check-equal? (num-adds (multC (plusC (numC 4) (numC 4)) (numC 4))) 1)
(check-equal? (num-adds (plusC (plusC (numC 3) (numC 4)) (plusC (numC 3) (numC 4)))) 3)
(check-equal? (num-adds (plusC (plusC (squareC (numC 3)) (numC 4)) (plusC (numC 3) (numC 4)))) 3)
(check-equal? (num-adds (multC (numC 3) (numC 4))) 0)





;parse
 ;accepts an s-expression code
 ;returns the parsed ArithC representation of code
(define (parse [code : Sexp]) : ArithC
  (match code
    [(? real? n)   (numC n)]
    [(list '+ l r) (plusC (parse l) (parse r))]
    [(list '* l r) (multC (parse l) (parse r))]
    [(list '^2 n)  (squareC (parse n))]
    [other (error 'parse "syntax error in ~e" other)]))

;tests
(check-equal? (parse '{+ 1 2}) (plusC (numC 1) (numC 2)))
(check-equal? (parse '{* 1 2}) (multC (numC 1) (numC 2)))
(check-equal? (parse '{^2 2}) (squareC (numC 2)))
(check-exn #rx"syntax error" (lambda () (parse '(+ 2))))





;top-interp
 ;accepts s-expression s
 ;returns the interpretation of the parse of s
(define (top-interp [s : Sexp]) : Real
  (interp (parse s)))

;tests
(check-equal? (top-interp '(* 2 3)) 6)
(check-equal? (top-interp '(+ 2 3)) 5)
(check-equal? (top-interp '(^2 2)) 4)
(check-equal? (top-interp '(+ (* (^2 5) 4) 100)) 200)
(check-exn #rx"syntax error" (lambda () (top-interp '(+ 2))))
(check-exn #rx"syntax error" (lambda () (top-interp '())))



