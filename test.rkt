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

(define get-operator car)
(define exp1 cadr)
(define exp2 caddr)

(define math-eval-cps
  (lambda (expr return)
    (cond 
      [(null? expr) (error 'undefined "ya know jimbo that's not a valid expression")]
      [(number? expr) (return expr)]
      ;; if next exprression's length is 2, it's unary -
      [(and (eq? (list-length expr) 2)
            (eq? (get-operator expr) '-))
       (math-eval-cps (exp1 expr) (lambda (v) (return (* -1 v))))]
      [(eq? (list-length expr) 2) (error 'undefined "ya know jimbo that's not a valid exprression")]
      [(not (eq? (list-length expr) 3)) (error 'undefined "ya know jimbo that's not a valid exprression")]
      [(eq? (get-operator expr) '+) 
       (math-eval-cps (exp1 expr) 
                 (lambda (v1) (math-eval-cps (exp2 expr) 
                                        (lambda (v2) (return (+ v1 v2))))))]
      [(eq? (get-operator expr) '-) 
       (math-eval-cps (exp1 expr) 
                 (lambda (v1) (math-eval-cps (exp2 expr) 
                                        (lambda (v2) (return (- v1 v2))))))]
      [(eq? (get-operator expr) '*) 
       (math-eval-cps (exp1 expr) 
                 (lambda (v1) (math-eval-cps (exp2 expr) 
                                        (lambda (v2) (return (* v1 v2))))))]
      [(eq? (get-operator expr) '/) 
       (math-eval-cps (exp1 expr) 
                 (lambda (v1) (math-eval-cps (exp2 expr) 
                                        (lambda (v2) (return (quotient v1 v2))))))]
      [(eq? (get-operator expr) '%) 
       (math-eval-cps (exp1 expr) 
                 (lambda (v1) (math-eval-cps (exp2 expr) 
                                        (lambda (v2) (return (remainder v1 v2))))))])))

(define math-eval
  (lambda (expr) 
    (math-eval-cps expr (lambda (v) v))))   

(define 
       
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

