#lang typed/racket
(require typed/rackunit)


;Arith - language definition
(define-type ExprC (U NumC BinopC IdC Ifleq0C))
(struct NumC    ([n : Real]) #:transparent)
(struct BinopC  ([operation : Symbol] [l : ExprC] [r : ExprC]) #:transparent)
(struct AppC    ([funid : Symbol] [param : ExprC]) #:transparent)
(struct IdC     ([id : Symbol]) #:transparent)
(struct Ifleq0C ([c : ExprC] [y : ExprC] [n : ExprC]) #:transparent)

;(struct FundefC)

;parse
 ;in: s-expression code
 ;out: the parsed ExprC representation of code
(define (parse [code : Sexp]) : ExprC
  (match code
    [(? real? n)   (NumC n)]
    [(? symbol? s) (IdC s)]
    [(list '+ l r) (BinopC '+ (parse l) (parse r))]
    [(list '- l r) (BinopC '- (parse l) (parse r))]
    [(list '* l r) (BinopC '* (parse l) (parse r))]
    [(list '/ l r) (BinopC '/ (parse l) (parse r))]
    [(list 'ifleq0? f s r) (Ifleq0C (parse f) (parse s) (parse r))]
    [other (error 'OAZO3-parse "syntax error in ~e" other)]))

;tests
(check-equal? (parse 1) (NumC 1))
(check-equal? (parse 'y) (IdC 'y))
(check-equal? (parse '{+ 1 2}) (BinopC '+ (NumC 1) (NumC 2)))
(check-equal? (parse '{- 1 2}) (BinopC '- (NumC 1) (NumC 2)))
(check-equal? (parse '{* 1 2}) (BinopC '* (NumC 1) (NumC 2)))
(check-equal? (parse '{/ 1 2}) (BinopC '/ (NumC 1) (NumC 2)))
(check-equal? (parse '{ifleq0? -1 5 3}) (Ifleq0C (NumC -1) (NumC 5) (NumC 3)))
(check-exn #rx"syntax error" (lambda () (parse '(+ 2))))



;interp
 ;in: ExprC a
 ;out: evaluation of a
(define (interp [a : ExprC]) : Real
  (match a
    [(NumC n) n]
    ;[(IdC s) s]
    [(BinopC '+ l r) (+ (interp l) (interp r))]
    [(BinopC '- l r)  (- (interp l) (interp r))]
    [(BinopC '* l r) (* (interp l) (interp r))]
    [(BinopC '/ l r)  (/ (interp l) (interp r))]
    [(Ifleq0C c y n) (cond [(<= (interp c) 0) (interp y)]
                           [else (interp n)])]))

;tests
(check-equal? (interp (BinopC '+ (NumC 3) (NumC 4))) 7)
(check-equal? (interp (BinopC '* (NumC 3) (NumC 4))) 12)
(check-equal? (interp (BinopC '- (NumC 3) (NumC 4))) -1)
(check-equal? (interp (BinopC '/ (NumC 3) (NumC 4))) 3/4)


 
;top-interp
 ;in: s-expression s
 ;out: the interpretation of the parse of s
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

