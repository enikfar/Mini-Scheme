#lang racket
;Ehsan Nikfar
;Zaina Saleem

(require rackunit)
(require "parse.rkt")

(provide parse-tests)

(define parse-tests
  (test-suite
   "Parse tests"
   ;;; lit-exp
   (test-pred "Literal"
              lit-exp?
              (parse 5))
   (test-equal? "get"
                (lit-exp-num (parse 22))
                22)
   ;;; var-exp
   (test-pred "var-exp"
              var-exp?
              (parse 'sym))
   (test-equal? "symbol"              
                (var-exp-symbol (parse 'sym))
                'sym)
   (test-exn "more than 1 input"
             exn:fail?
             (λ () (parse 'sym 5)))
   ;;; app-exp
   (test-exn "empty"
             exn:fail?
             (λ () (parse '())))
   (test-equal? "procedure"
                (parse '(foo))
                (app-exp (var-exp 'foo) '()))
   (test-pred "procedure 2"
              var-exp?
              (app-exp-proc (parse '(foo 1 2 3))))
   (test-pred "arguments of app-exp is list"
              list?
              (app-exp-args (parse '(foo 1 2 3))))
   (test-equal? "proc with arg"
                (parse '(foo x y z))
                (app-exp (var-exp 'foo) (list (var-exp 'x) (var-exp 'y) (var-exp 'z))))
   (test-equal? "proc with constant"
                (parse '(barz 1))
                (app-exp (var-exp 'barz) (list (lit-exp 1))))
   ;;; ite-exp
   (test-equal? "con statement with eqv?"
                (parse '(if (eqv? 1 2) 5 6))
                (ite-exp (app-exp (var-exp 'eqv?) (list (lit-exp 1) (lit-exp 2))) (lit-exp 5) (lit-exp 6)))
   (test-equal? "cond statement with symbol"
                (parse '(if (number? y) x 6))
                (ite-exp (app-exp (var-exp 'number?) (list (var-exp 'y))) (var-exp 'x) (lit-exp 6)))
   (test-exn "cond without enough elements"
             exn:fail?
             (λ () (parse '(if (eqv? 1 2) 5))))
   ;;; let-exp
   (test-equal? "let with numbers"
                (parse '(let ([a 1] [b 5])(+ a b)))
                (let-exp '(a b) (list (lit-exp 1) (lit-exp 5)) (app-exp (var-exp '+) (list (var-exp 'a) (var-exp 'b)))))
   (test-equal? "let expressions with = symbols"
                (parse '(let ([a x]  [b y])  a))
                (let-exp '(a b) (list (var-exp 'x) (var-exp 'y)) (var-exp 'a)))
   (test-exn "let expressions w/o elements"
             exn:fail?
             (λ () (parse '(let ([a x]  [b y]) ))))
   ))
