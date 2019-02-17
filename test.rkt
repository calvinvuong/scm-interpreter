(require "simpleParser.rkt")
(parser "testcode")

(define list-length-cps
  (lambda (lis return)
    (cond
      [(null? lis) (return 0)]
      [else (list-length-cps (cdr lis) (lambda (v) (return (+ 1 v))))])))

(define list-length
  (lambda (lis)
    (list-length-cps lis (lambda (v) v))))

(define mathEval 
  (lambda (exp return)
    (cond 
      [(null? exp) (error 'undefined "Well hey jimbo that's not a valid expression")]
      [(number? exp) (return exp)]
      ;; if next expression's length is 2, it's unary -
      [(and (eq? (list-length lis) 2)
            (eq? (car lis) '-))
       (mathEval (cdr exp) (lambda (v) (return (* -1 v))))
      [(eq? (list-length lis) 2) (error 'undefined "Well hey jimbo that's not a valid expression")]
      [(not (eq? 
       
;; Add a binding to the state
(define add-to-state
  (lambda (var val state)
    (cons (var val) state)))

; Remove binding from state
(define remove-from-state
  (lambda (var state return)
    (cond
      [(null? state)        (return '())]
      [(eq? (car state) a)  (return (cdr state))]
      [else                 (remove-from-state var (cdr state) (lambda (v) (return (cons (car lis) v))))])))

(define  )

