#lang typed/racket
(require typed/rackunit)


;Arith - language definition
;expressions
(define-type ExprC (U NumC BinopC IdC Ifleq0C))
(struct NumC    ([n : Real]) #:transparent)                                   ;numbers
(struct BinopC  ([operation : Symbol] [l : ExprC] [r : ExprC]) #:transparent) ;+-*/
(struct AppC    ([funid : Symbol] [param : ExprC]) #:transparent)             ;function call
(struct IdC     ([id : Symbol]) #:transparent)                                ;variable
(struct Ifleq0C ([c : ExprC] [y : ExprC] [n : ExprC]) #:transparent)          ;simple conditional
;functions
(struct FundefC ([kw : Symbol] [funid : IdC] [param : IdC] [colon : Symbol] [exp : ExprC]) #:transparent)





;function definitions for a runable program
(define funs '{
              {func adder param : {+ 1 2}}
              {func suber x : {+ 1 x}}
              })





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
    [other (error 'parse "OAZO3 syntax error in ~e" other)]))
;tests
(check-equal? (parse 1) (NumC 1))
(check-equal? (parse 'y) (IdC 'y))
(check-equal? (parse '{+ 1 2}) (BinopC '+ (NumC 1) (NumC 2)))
(check-equal? (parse '{- 1 2}) (BinopC '- (NumC 1) (NumC 2)))
(check-equal? (parse '{* 1 2}) (BinopC '* (NumC 1) (NumC 2)))
(check-equal? (parse '{/ 1 2}) (BinopC '/ (NumC 1) (NumC 2)))
(check-equal? (parse '{ifleq0? -1 5 3}) (Ifleq0C (NumC -1) (NumC 5) (NumC 3)))
(check-exn #rx"syntax error" (lambda () (parse '(+ 2))))


;parse-fundef
 ;in: s-expression code
 ;out: the parsed FundefC representation of code
(define (parse-fundef [code : Sexp]) : FundefC
  (match code
    [(list 'func (? symbol? n) (? symbol? p) ': e) (FundefC 'func (cast (parse n) IdC) (cast (parse p) IdC) ': (parse e))]
    [other (error 'parse-fundef "OAZO3 syntax error in ~e" other)]))
;tests
(check-equal? (parse-fundef '{func name param : {+ 1 2}})
              (FundefC 'func (IdC 'name) (IdC 'param) ': (BinopC '+ (NumC 1) (NumC 2))))
(check-exn #rx"syntax error" (lambda () (parse-fundef '{notafunc name param : {+ 1 2}})))
(check-exn #rx"syntax error" (lambda () (parse-fundef '(+ 2))))


;parse-prog
 ;in: s-expression code
 ;out: the parsed program (list of FundefC)
(define (parse-prog [code : Sexp]) : (Listof FundefC)
  (match code
    ['() '()]
    [(cons f r) (cons (parse-fundef f) (parse-prog r))]))
;tests
(check-equal? (parse-prog funs)
              (list
               (FundefC 'func (IdC 'adder) (IdC 'param) ': (BinopC '+ (NumC 1) (NumC 2)))
               (FundefC 'func (IdC 'suber) (IdC 'x) ': (BinopC '+ (NumC 1) (IdC 'x)))))




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

