#lang racket
;Ehsan Nikfar
;Zaina Saleem
(provide parse
         lit-exp
         lit-exp?
         lit-exp-num
         var-exp
         var-exp?
         var-exp-symbol
         app-exp
         app-exp?
         app-exp-proc
         app-exp-args
         ite-exp
         ite-exp?
         ite-exp-cond
         ite-exp-then
         ite-exp-else
         let-exp
         let-exp?
         let-exp-exps
         let-exp-vals
         let-exp-proc
         lambda-exp
         lambda-exp?
         lambda-exp-exp
         lambda-exp-params
         box-val
         box?
         set-exp
         set-exp-sys
         set-exp-exp
         set-exp?
         begin-exp
         begin-exp-exps
         begin-exp?
         )



(struct lit-exp (num) #:transparent)


(struct var-exp (symbol) #:transparent)


(struct app-exp (proc args) #:transparent)
;(struct app-exp (proc1 args proc2) #:transparent)


(struct ite-exp (cond then else) #:transparent)

(struct box (val) #:transparent)

(struct set-exp ( sys exp) #:transparent)

(struct let-exp (exps vals proc) #:transparent)

(struct lambda-exp ( params exp) #:transparent)
(struct begin-exp ( exps) #:transparent)



(define (parse input)
  (letrec ([parse-error (λ () (error 'parse "Invalid syntax ~s" input))])
    (cond [(number? input) (lit-exp input)]
          [(symbol? input) (var-exp input)]
          [(list? input)
           (cond [(empty? input) (parse-error)]
                 [(equal? 'if (first input)) (cond [(equal? 4 (length input)) (ite-exp (parse (second input))
                                                                                       (parse (third input))
                                                                                       (parse (fourth input)))]
                                                   [else (parse-error)])]
                 [(equal? 'let (first input)) (cond [(equal? 3 (length input)) (let-exp (map first (second input))
                                                                                        (map parse (map second (second input)))
                                                                                        (parse (third input)))]
                                                    [else (parse-error)])]
                 [ (equal? 'lambda ( first input)) (cond [(equal? 3 (length input)) ( lambda-exp (second input) ( parse ( third input)))])]
                 [ ( equal? 'set! ( first input))  ( cond [(equal? 3 ( length input)) ( set-exp ( second input) ( parse (third input)))])]
                 [ (equal? 'begin ( first input)) ( begin-exp ( map parse ( rest input)))]
                 
                
                [else (app-exp (parse (first input)) (map parse (rest input)))])]
                 
          
          [else (parse-error)])))

