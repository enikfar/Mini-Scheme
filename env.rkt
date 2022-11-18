#lang racket
;Ehsan Nikfar
;Zaina Saleem
(provide env
         env?
         empty-env
         empty-env?
         env-syms
         env-vals
         env-previous
         env-lookup-helper
         env-lookup
         )

; The empty environment is null.
(define empty-env null)
(define empty-env? null?)


;( define (env vars vals envv)
 ; ( cons 'env  ( cons vars ( cons vals (list envv)))))

(struct env (syms vals previous) #:transparent)
                           
;(define ( env? obj)
;  ( and ( list? obj)
;        ( not ( empty? obj))
;        ( eq? ( first obj) 'env)
;        ( equal? 4 (length obj))
;
;
;
;        )
;  )

;(define ( env-vals env)
;  (cond[( empty-env? env) empty]
;  [else
;  ( third env)]))
;( define (env-previous obj)
;   ( last obj))
;( define ( env-syms env)
;   (cond[( empty-env? env) empty]
;  [else
;   ( second env)]))


                    
                   
  
  
(define env-a
  (env '(x y) '(1 2) empty-env))
(define env-b
  (env '(x z) '(5 7) env-a))
(define env-c
  (env '(x z a b c) '(5 7 1 2 3) env-a))

( define (env-lookup-helper vars vals symbol)
   (cond [( empty? vars) #f]
         [ ( eq? ( first vars)  symbol)
            ( first vals)]
         [ else
           ( env-lookup-helper ( rest vars) ( rest vals) symbol)]))
   



;( define (env-lookup env symbol)
; (cond[(empty-env? env) (error 'env-lookup "No binding for ~s" symbol)]
;        [ else
;         (cond[ ( equal? #f (env-lookup-helper ( second env) ( third env) symbol))
;                 (env-lookup (last env) symbol)]
;                  [ else
;                     (env-lookup-helper ( second env) ( third env) symbol)])]))

(define (extended-env? e)
(and (list? e)
(not (null? e))
(eq? (first e) 'env)))

(define (env-lookup environment symbol)
(let ([vals (env-vals environment)]
[syms (env-syms environment)]
[prev (env-previous environment)])
(cond
[(empty? syms)
(if (empty-env? prev)
(error 'env-lookup "No binding for ~s" symbol)
(env-lookup prev symbol)) ]
[(equal? (car syms) symbol) (car vals)]
[else (env-lookup (env (cdr syms) (cdr vals) prev) symbol)])))



; test code
;( let* ( [v 0]
;         [ f ( lambda(x)
;                ( set! v ( + v 1))
;                x)]
;         )
;   ( f ( + v 5)))



( let ([ a 1])
   ( let ( [ b (lambda (x) ( begin ( set! a ( + 1 a)) a))])
      ( +  ( b  0) a)))

(let ([ x 5])
       (let([ y  ( list ( begin ( set! x 10) x))])
             ( cons x y)))

;( let* ( [ x 10])
;        [ f ( lambda (x) ( * x x))]
;          ( f ( - x 5)))

;(define (egg x)
;  (if (zero? x)
;      a
;      (let ([a (* a x)])
;        (egg (sub1 x)))))
;(define (ham x)
;  (let ([a 1])
;    (egg x)))

( let* ([ z 7]
       [ f 2]
       [  myfunc ( lambda(x) (set! z ( * z f)) x)])
         ( myfunc ( + z 1)))




   
