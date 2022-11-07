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
         )



(struct lit-exp (num) #:transparent)


(struct var-exp (symbol) #:transparent)


(struct app-exp (proc args) #:transparent)


(struct ite-exp (cond then else) #:transparent)



(struct let-exp (exps vals proc) #:transparent)


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
                 [else (app-exp (parse (first input)) (map parse (rest input)))])]
          [else (parse-error)])))

