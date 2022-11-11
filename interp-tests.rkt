#lang racket

(require rackunit)
(require "env.rkt" "parse.rkt" "interp.rkt")

(provide interp-tests)

(define test-env
  (env '(x y) '(10 23) init-env))

(define interp-tests
  (test-suite
   "Interpreter tests"
   
   (test-exn "invalid input"
             exn:fail?
             (λ () (eval-exp 5 test-env)))
   (test-eqv? "number"
              (eval-exp (lit-exp 5) test-env)
              5)
   
   (test-eqv? "valid symbol in env"
              (eval-exp (var-exp 'x) test-env)
              10)
   (test-exn "invalid symbol in env"
             exn:fail?
             (λ () (eval-exp (var-exp 'z) test-env)))
   
 
   (test-eqv? "+ operator for nothing"
              (eval-exp (app-exp (var-exp '+)
                                 '()) test-env)
              0)
   (test-eqv? "+ for constants"
              (eval-exp (app-exp (var-exp '+)
                                 (list (lit-exp 1) (lit-exp 2))) test-env)
              3)
   (test-eqv? "+ 2"
              (eval-exp (app-exp (var-exp '+)
                                 (list (lit-exp 1) (lit-exp 2) (lit-exp 3) (lit-exp 4))) test-env)
              10)
   (test-eqv? "+ for numbers and env var"
              (eval-exp (app-exp (var-exp '+)
                                 (list (lit-exp 4) (lit-exp 2) (var-exp 'x))) test-env)
              16)
  
   (test-eqv? "- operator in test-env"
              (eval-exp (app-exp (var-exp '-)
                                 (list (var-exp 'y) (var-exp 'x))) test-env)
              13)
   (test-exn "- for nothing"
             exn:fail?
             (λ () (eval-exp (app-exp (var-exp '-)
                                      '()) test-env)))
   (test-eqv? "- operator for multiple numbers"
              (eval-exp (app-exp (var-exp '-)
                                 (list (lit-exp 1) (lit-exp 2) (lit-exp 3) (lit-exp 4))) test-env)
              -8)
   ;; multiply
   (test-eqv? "* operator in init-env"
              (eval-exp (app-exp (var-exp '*)
                                 (list (var-exp 'y) (var-exp 'x))) init-env)
              966)
   (test-eqv? "* operator for nothing"
              (eval-exp (app-exp (var-exp '*)
                                 '()) test-env)
              1)
   (test-eqv? "* operator for multiple numbers"
              (eval-exp (app-exp (var-exp '*)
                                 (list (lit-exp 1) (lit-exp 2) (lit-exp 3) (lit-exp 4))) test-env)
              24)
  
   (test-eqv? "/ operator in test-env"
              (eval-exp (app-exp (var-exp '/)
                                 (list (var-exp 'y) (var-exp 'x))) test-env)
              (/ 23 10))
   (test-exn "/ for nothing"
             exn:fail?
             (λ () (eval-exp (app-exp (var-exp '/)
                                      '()) test-env)))
   (test-eqv? "/ operator for multiple numbers"
              (eval-exp (app-exp (var-exp '/)
                                 (list (lit-exp 10) (lit-exp 2) (lit-exp 1) (lit-exp 4) (lit-exp 5))) test-env)
              (/ 1 4))
   
   (test-eqv? "add1 w number"
              (eval-exp (app-exp (var-exp 'add1)
                                 (list (lit-exp 10))) test-env)
              11)
   (test-exn "add1 for nothing"
             exn:fail?
             (λ () (eval-exp (app-exp (var-exp 'add1)
                                      '()) test-env)))
   (test-exn "add1 for multiple number"
             exn:fail?
             (λ () (eval-exp (app-exp (var-exp 'add1)
                                      (list (lit-exp 10) (lit-exp 9) (lit-exp 'x))) test-env)))
   (test-eqv? "add1 for env variable"
              (eval-exp (app-exp (var-exp 'add1)
                                 (list (var-exp 'y))) test-env)
              24)
  
   (test-eqv? "sub1 for number"
              (eval-exp (app-exp (var-exp 'sub1)
                                 (list (lit-exp 10))) test-env)
              9)
   (test-exn "sub1 for nothing"
             exn:fail?
             (λ () (eval-exp (app-exp (var-exp 'sub1)
                                      '()) test-env)))
   (test-exn "sub1 for multiple number"
             exn:fail?
             (λ () (eval-exp (app-exp (var-exp 'sub1)
                                      (list (lit-exp 10) (lit-exp 9) (lit-exp 'x))) test-env)))
   (test-eqv? "sub1 for env variable"
              (eval-exp (app-exp (var-exp 'sub1)
                                 (list (var-exp 'y))) test-env)
              22)
  
   (test-eqv? "negate for number"
              (eval-exp (app-exp (var-exp 'negate)
                                 (list (lit-exp 9))) test-env)
              -9)
   (test-exn "negate for nothing"
             exn:fail?
             (λ () (eval-exp (app-exp (var-exp 'negate)
                                      '()) test-env)))
   (test-exn "negate for multiple numbers"
             exn:fail?
             (λ () (eval-exp (app-exp (var-exp 'negate)
                                      (list (lit-exp 10) (lit-exp 9) (lit-exp 'x))) test-env)))
   (test-eqv? "negate for env variable"
              (eval-exp (app-exp (var-exp 'negate)
                                 (list (var-exp 'y))) test-env)
              -23)
  
   (test-equal? "list for number and env variable"
                (eval-exp (app-exp (var-exp 'list)
                                   (list (lit-exp 9) (var-exp 'x))) test-env)
                '(9 10))
   (test-equal? "empty list"
                (eval-exp (app-exp (var-exp 'list)
                                   '()) test-env)
                '())
   (test-equal? "nested lists"
                (eval-exp (app-exp (var-exp 'list)
                                   (list (app-exp (var-exp 'list) (list (lit-exp 8) (lit-exp 9))) (lit-exp 10))) test-env)
                '((8 9) 10))
   
   (test-equal? "cons for number and env variable"
                (eval-exp (app-exp (var-exp 'cons)
                                   (list (lit-exp 9) (var-exp 'x))) test-env)
                (cons 9 10))
   (test-exn "cons for 1 number"
             exn:fail?
             (λ () (eval-exp (app-exp (var-exp 'cons)
                                      (list (lit-exp 9))) test-env)))
   (test-exn "cons for 3 or more numbers"
             exn:fail?
             (λ () (eval-exp (app-exp (var-exp 'cons)
                                      (list (lit-exp 9) (lit-exp 16) (var-exp 'x))) test-env)))
   (test-equal? "make a list with cons"
                (eval-exp (app-exp (var-exp 'cons)
                                   (list (lit-exp 1) (app-exp (var-exp 'cons)
                                                              (list (lit-exp 2) (app-exp (var-exp 'cons)
                                                                                         (list (lit-exp 3) (var-exp 'null))))))) test-env)
                '(1 2 3))
  
   (test-equal? "car 1"
                (eval-exp (app-exp (var-exp 'car)
                                   (list (app-exp (var-exp 'cons) (list (var-exp 'x) (lit-exp 9))))) test-env)
                10)
   (test-equal? "car 2"
                (eval-exp (app-exp (var-exp 'car)
                                   (list (app-exp (var-exp 'list) (list (lit-exp 8) (lit-exp 9) (lit-exp 10))))) test-env)
                8)
   (test-equal? "car 3"
                (eval-exp (app-exp (var-exp 'car)
                                   (list (app-exp (var-exp 'list) (list (app-exp (var-exp 'list) (list (lit-exp 8) (var-exp 'y))) (lit-exp 9))))) test-env)
                '(8 23))
   (test-equal? "car 4"
                (eval-exp (app-exp (var-exp 'car)
                                   (list (app-exp (var-exp 'cons) (list (lit-exp 1) (lit-exp 2))))) test-env)
                1)
   (test-exn "car 5"
             exn:fail?
             (λ () (eval-exp (app-exp (var-exp 'car) (list (lit-exp 1) (lit-exp 2))) test-env)))
   (test-exn "car takes an empty list"
             exn:fail?
             (λ () (eval-exp (app-exp (var-exp 'car) (list (var-exp 'null))) test-env)))
  
   (test-equal? "cdr 1"
                (eval-exp (app-exp (var-exp 'cdr)
                                   (list (app-exp (var-exp 'list) (list (lit-exp 1) (lit-exp 2) (var-exp 'x))))) test-env)
                '(2 10))
   (test-equal? "cdr 2"
                (eval-exp (app-exp (var-exp 'cdr)
                                   (list (app-exp (var-exp 'list) (list (lit-exp 1))))) test-env)
                '())
   (test-equal? "cdr 3"
                (eval-exp (app-exp (var-exp 'cdr)
                                   (list (app-exp (var-exp 'list) (list (lit-exp 1) (app-exp (var-exp 'list) (list (lit-exp 2) (lit-exp 3))))))) test-env)
                '((2 3)))
   (test-equal? "cdr 4"
                (eval-exp (app-exp (var-exp 'cdr)
                                   (list (app-exp (var-exp 'cons) (list (lit-exp 1) (lit-exp 2))))) test-env)
                2)
   (test-exn "cdr 5"
             exn:fail?
             (λ () (eval-exp (app-exp (var-exp 'cdr) (list (lit-exp 1) (lit-exp 2))) test-env)))
   (test-exn "cdr w an empty list"
             exn:fail?
             (λ () (eval-exp (app-exp (var-exp 'cdr) (list (var-exp 'null))) test-env)))
  
   (test-equal? "false output of eqv?"
                (eval-exp (app-exp (var-exp 'eqv?) (list (lit-exp 1) (lit-exp 2))) test-env)
                'False)
   (test-equal? "true output of eqv?"
                (eval-exp (app-exp (var-exp 'eqv?) (list (lit-exp 2) (lit-exp 2))) test-env)
                'True)
   (test-exn "eqv? takes only 1 argument"
             exn:fail?
             (λ () (eval-exp (app-exp (var-exp 'eqv?) (list (lit-exp 2))) test-env)))
   (test-exn "eqv? takes more than 2 arguments"
             exn:fail?
             (λ () (eval-exp (app-exp (var-exp 'eqv?) (list (lit-exp 2) (lit-exp 3) (lit-exp 4))) test-env)))
   
   (test-equal? "false output of lt?"
                (eval-exp (app-exp (var-exp 'lt?) (list (lit-exp 2) (lit-exp 1))) test-env)
                'False)   
   
   (test-equal? "true output of gt?"
                (eval-exp (app-exp (var-exp 'gt?) (list (lit-exp 2) (lit-exp 1))) test-env)
                'True)
   
   (test-equal? "true output of leq?"
                (eval-exp (app-exp (var-exp 'leq?) (list (lit-exp 2) (lit-exp 2))) test-env)
                'True)
   
   (test-equal? "true output of leq?"
                (eval-exp (app-exp (var-exp 'geq?) (list (lit-exp 1) (lit-exp 2))) test-env)
                'False)
   
   (test-equal? "true output w null"
                (eval-exp (parse '(null? null)) test-env)
                'True)
   (test-equal? "False output w null?"
                (eval-exp (parse '(null? 23)) test-env)
                'False)
   (test-exn "null? takes more than 1 argument"
             exn:fail?
             (λ () (eval-exp (app-exp (var-exp 'null?) (list (lit-exp 1) (lit-exp 2))) test-env)))
   
   (test-equal? "true output of list?"
                (eval-exp (app-exp (var-exp 'list?) (list (app-exp (var-exp 'list) (list (lit-exp 1) (lit-exp 2) (lit-exp 3))))) test-env)
                'True)
   (test-equal? "false output of list?"
                (eval-exp (parse '(list? 23)) test-env)
                'False)
   (test-exn "list? takes more than 1 argument"
             exn:fail?
             (λ () (eval-exp (app-exp (var-exp 'list?) (list (lit-exp 1) (lit-exp 2))) test-env)))
   
   (test-equal? "true output of number?"
                (eval-exp (parse '(number? 23)) test-env)                
                'True)
   (test-equal? "false output of number?"
                (eval-exp (app-exp (var-exp 'number?) (list (app-exp (var-exp 'list) (list (lit-exp 1) (lit-exp 2) (lit-exp 3))))) test-env)
                'False)
   (test-exn "number? takes more than 1 argument"
             exn:fail?
             (λ () (eval-exp (app-exp (var-exp 'number?) (list (lit-exp 1) (lit-exp 2))) test-env)))
  
   (test-equal? "cond then branch"
                (eval-exp (ite-exp (app-exp (var-exp 'number?) (list (lit-exp 2)))
                                   (lit-exp 100)
                                   (lit-exp 1)) test-env)
                100)
   (test-equal? "cond execute else branch"
                (eval-exp (ite-exp (app-exp (var-exp 'gt?) (list (var-exp 'x) (var-exp 'y)))
                                   (app-exp (var-exp 'add1) (list (var-exp 'x)))
                                   (app-exp (var-exp 'add1) (list (var-exp 'y)))) test-env)
                24)
  
   (test-equal? "single let "
                (eval-exp (let-exp '(a b) (list (lit-exp 1) (lit-exp 5))
                                   (app-exp (var-exp '+) (list (var-exp 'a) (var-exp 'b)))) test-env)
                6)
   (test-equal? "Nested let "
                (eval-exp (let-exp '(a b) (list (app-exp (var-exp '*) (list (lit-exp 2) (lit-exp 3))) (lit-exp 24))
                                   (let-exp '(c) (list (app-exp (var-exp '-) (list (var-exp 'b) (var-exp 'a))))
                                            (app-exp (var-exp '*) (list (var-exp 'c) (app-exp (var-exp '+) (list (var-exp 'a) (var-exp 'b))))))) test-env)
                540)
   ))





























