#lang typed/racket
(require typed/rackunit)


;Arith - language definition
;expressions
(define-type ExprC (U NumC BinopC IdC Ifleq0C AppC))
(struct NumC    ([n : Real]) #:transparent)                                   ;numbers
(struct BinopC  ([operation : Symbol] [l : ExprC] [r : ExprC]) #:transparent) ;+-*/
(struct AppC    ([fun : Symbol] [arg : ExprC]) #:transparent)             ;function call
(struct IdC     ([id : Symbol]) #:transparent)                                ;variable
(struct Ifleq0C ([c : ExprC] [y : ExprC] [n : ExprC]) #:transparent)          ;simple conditional
;functions
(struct FundefC ([name : Symbol] [arg : Symbol] [body : ExprC]) #:transparent)





;function definitions for a runable program
(define funs '{
               {func {adder param} : {+ 1 2}}
               {func {suber x} : {- 1 x}}
              })

(define test '{
               {func {f x} : {+ x 14}}
               {func {main init} : {f 2}}
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
    [(list name param) (AppC (cast name Symbol) (parse param))]
    [other (error 'parse "OAZO3 syntax error in ~e" other)]))
;tests
(check-equal? (parse 1) (NumC 1))
(check-equal? (parse 'y) (IdC 'y))
(check-equal? (parse '{+ 1 2}) (BinopC '+ (NumC 1) (NumC 2)))
(check-equal? (parse '{- 1 2}) (BinopC '- (NumC 1) (NumC 2)))
(check-equal? (parse '{* 1 2}) (BinopC '* (NumC 1) (NumC 2)))
(check-equal? (parse '{/ 1 2}) (BinopC '/ (NumC 1) (NumC 2)))
(check-equal? (parse '{f 10}) (AppC 'f (NumC 10)))
(check-equal? (parse '{ifleq0? -1 5 3}) (Ifleq0C (NumC -1) (NumC 5) (NumC 3)))
(check-exn #rx"syntax error" (lambda () (parse '(+ 2 2 2))))


;parse-fundef
 ;in: s-expression code
 ;out: the parsed FundefC representation of code
(define (parse-fundef [code : Sexp]) : FundefC
  (match code
    [(list 'func (list (? symbol? n) (? symbol? p)) ': e) (FundefC n p (parse e))]
    [other (error 'parse-fundef "OAZO3 syntax error in ~e" other)]))
;tests
(check-equal? (parse-fundef '{func (name arg) : {+ 1 2}})
              (FundefC 'name 'arg (BinopC '+ (NumC 1) (NumC 2))))
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
               (FundefC 'adder 'param (BinopC '+ (NumC 1) (NumC 2)))
               (FundefC 'suber 'x (BinopC '- (NumC 1) (IdC 'x)))))
(check-exn #rx"syntax error" (lambda () (parse-prog '(+ 2))))






(define (subst [what : ExprC] [for : Symbol] [in : ExprC]) : ExprC
  (match in
    [(NumC n) in]
    [(IdC s) (cond
               [(symbol=? s for) what]
               [else in])]
    [(AppC f a) (AppC f (subst what for a))]
    [(BinopC '+ l r) (BinopC '+ (subst what for l)
                        (subst what for r))]
    [(BinopC '* l r) (BinopC '* (subst what for l)
                        (subst what for r))]
    [(BinopC '- l r) (BinopC '- (subst what for l)
                        (subst what for r))]
    [(BinopC '/ l r) (BinopC '/ (subst what for l)
                        (subst what for r))]))

(define (get-fundef [n : symbol] [fds : (listof FundefC)]) : FundefC
    (cond
      [(empty? fds) (error 'get-fundef "reference to undefined function")]
      [(cons? fds) (cond
                     [(equal? n (fdC-name (first fds))) (first fds)]
                     [else (get-fundef n (rest fds))])]))


;interp
 ;in: ExprC exp, list of FundefC lst
 ;out: evaliation of exp as a Real
(define (interp [exp : ExprC] [lst : (Listof FundefC)]) : Real
  (match exp
    [(NumC n) n]
    [(BinopC '+ l r) (+ (interp l lst) (interp r lst))]
    [(BinopC '- l r)  (- (interp l lst) (interp r lst))]
    [(BinopC '* l r) (* (interp l lst) (interp r lst))]
    [(BinopC '/ l r)  (/ (interp l lst) (interp r lst))]
    [(Ifleq0C c y n) (cond [(<= (interp c lst) 0) (interp y lst)]
                           [else (interp n lst)])]
    [(AppC funid param) 0]
    [(IdC s) 0]))
;tests



;interp-fns
 ;in: list of FundefC funcs
 ;out: evaluation of funtions as a Real
(define (interp-fns [funcs : (Listof FundefC)]) : Real
  0)
;tests
;(check-equal? (interp-fns) 0)


;top-interp




#|

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

|#