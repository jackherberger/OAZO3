#lang typed/racket
(require typed/rackunit)


;Arith - language definition
;expressions
(define-type ExprC (U NumC BinopC IdC Ifleq0C AppC))
(struct NumC    ([n : Real]) #:transparent)                                   ;numbers
(struct BinopC  ([operation : Symbol] [l : ExprC] [r : ExprC]) #:transparent) ;+-*/
(struct AppC    ([fun : Symbol] [arg : ExprC]) #:transparent)                 ;function call
(struct IdC     ([id : Symbol]) #:transparent)                                ;variable
(struct Ifleq0C ([c : ExprC] [y : ExprC] [n : ExprC]) #:transparent)          ;simple conditional
;functions
(struct FundefC ([name : Symbol] [arg : Symbol] [body : ExprC]) #:transparent)

;function definitions for a runable program
(define test0 '{{func {main init} : {+ 1 2}}})
(define test1 '{{func {f x} : {+ x 14}}
               {func {main init} : {f 2}}})
(define test2 '{{func {adder y} : {+ y 2}}
               {func {suber x} : {adder x}}
               {func {main param} : {suber 10}}})






;top-interp 
 ;in: list of oazo3 syntax functions fun-sexps
 ;out: the evaluation of main function in fun-sexps
(define (top-interp [fun-sexps : Sexp]) : Real
  (interp-fns (parse-prog fun-sexps)))


;interp-fns
 ;in: list of FundefC funcs
 ;out: evaluation of funtions as a Real
(define (interp-fns [funs : (Listof FundefC)]) : Real
  (define main (get-fundef 'main funs))
  (interp (FundefC-body main) funs))

;get-fundef
 ;in: a symbol n and list of FundefC fds
 ;out: the function in fds with name n
(define (get-fundef [n : Symbol] [fds : (Listof FundefC)]) : FundefC
    (cond
      [(empty? fds) (error 'get-fundef "OAZO3 reference to undefined function")]
      [(cons? fds) (cond
                     [(equal? n (FundefC-name (first fds))) (first fds)]
                     [else (get-fundef n (rest fds))])]))

;interp
 ;in: ExprC exp, list of FundefC lst
 ;out: evaluation of exp as a Real
(define (interp [exp : ExprC] [funs : (Listof FundefC)]) : Real
  (match exp
    [(NumC n) n]
    [(BinopC '+ l r) (+ (interp l funs) (interp r funs))]
    [(BinopC '- l r)  (- (interp l funs) (interp r funs))]
    [(BinopC '* l r) (* (interp l funs) (interp r funs))]
    [(BinopC '/ l r)  (/ (interp l funs) (interp r funs))]
    [(Ifleq0C c y n) (cond [(<= (interp c funs) 0) (interp y funs)]
                           [else (interp n funs)])]
    [(AppC f a) (define fd (get-fundef f funs))
                (interp (subst a
                               (FundefC-arg fd)
                               (FundefC-body fd))
                        funs)]
    [(IdC _) (error 'interp "OAZO3 shouldn't get here")]))

;subst
 ;in: ExprC what, Symbol for, and ExprC in
 ;out: 
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


;parse-prog
 ;in: s-expression code
 ;out: the parsed program (list of FundefC)
(define (parse-prog [code : Sexp]) : (Listof FundefC)
  (match code
    ['() '()]
    [(cons f r) (cons (parse-fundef f) (parse-prog r))]))

;parse-fundef
 ;in: s-expression code
 ;out: the parsed FundefC representation of code
(define (parse-fundef [code : Sexp]) : FundefC
  (match code
    [(list 'func (list (? symbol? n) (? symbol? p)) ': e) (FundefC n p (parse e))]
    [other (error 'parse-fundef "OAZO3 syntax error in ~e" other)]))

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







;parse-tests
(check-equal? (parse 1) (NumC 1))
(check-equal? (parse 'y) (IdC 'y))
(check-equal? (parse '{+ 1 2}) (BinopC '+ (NumC 1) (NumC 2)))
(check-equal? (parse '{- 1 2}) (BinopC '- (NumC 1) (NumC 2)))
(check-equal? (parse '{* 1 2}) (BinopC '* (NumC 1) (NumC 2)))
(check-equal? (parse '{/ 1 2}) (BinopC '/ (NumC 1) (NumC 2)))
(check-equal? (parse '{f 10}) (AppC 'f (NumC 10)))
(check-equal? (parse '{ifleq0? -1 5 3}) (Ifleq0C (NumC -1) (NumC 5) (NumC 3)))
(check-exn #rx"syntax error" (lambda () (parse '(+ 2 2 2))))



;tests-parse-fundef
(check-equal? (parse-fundef '{func (name arg) : {+ 1 2}})
              (FundefC 'name 'arg (BinopC '+ (NumC 1) (NumC 2))))
(check-exn #rx"syntax error" (lambda () (parse-fundef '{notafunc name param : {+ 1 2}})))
(check-exn #rx"syntax error" (lambda () (parse-fundef '(+ 2))))


;parse prog tests
#;(check-equal? (parse-prog funs)
              (list
               (FundefC 'adder 'param (BinopC '+ (NumC 1) (NumC 2)))
               (FundefC 'suber 'x (BinopC '- (NumC 1) (IdC 'x)))))
(check-exn #rx"syntax error" (lambda () (parse-prog '(+ 2))))




