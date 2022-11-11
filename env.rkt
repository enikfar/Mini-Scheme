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

(define (extended-env? e)
(and (list? e)
(not (null? e))
(eq? (first e) 'env)))
                    
                   
  
  
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



(define (env-lookup e sym)
(cond [(not (symbol? sym)) (error 'env-lookup "sym is not a symbol")]
[(empty-env? e) (error 'env-lookup "no binding for ~s" sym)]
[(extended-env? e)
(let ([idx (index-of (env-syms e) sym)])
(if idx
(list-ref (env-vals e) idx)
(env-lookup (env-previous e) sym)))]
[else (error 'env-lookup "e not an env")]))




      





   
