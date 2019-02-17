#lang racket

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



;; Add a binding to the state
;; takes a list of two elements: key value pair
;; max 1 recursion: cps not necessary
(define add-to-state
  (lambda (binding state)
    (if (null? state)
        '()
        (cons (cons (car binding) (car state)) (add-to-state (cdr binding) (cdr state))))))
      

;; Remove binding from state
;; wrapper function
(define remove-from-state
  (lambda (var state)
    (remove-from-state-helper var (car state) (cadr state) (lambda (v) v))))

;; s1 are the variable names
;; s2 are the values
(define remove-from-state-helper
  (lambda (var s1 s2 return)
    (cond
      [(null? s1)         (return '( () () ))]
      [(eq? (car s1) var) (remove-from-state-helper var (cdr s1) (cdr s2) (lambda (v) (return (cons (car v) (list (cadr v))))))]
      [else               (remove-from-state-helper var (cdr s1) (cdr s2) (lambda (v) (return (cons (cons (car s1) (car v)) (list (cons (car s2) (cadr v)))))))])))

;; update a variable value in state
;; wrapper function
(define update-binding
  (lambda (var val state)
    (update-binding-helper var val (car state) (cadr state) (lambda (v) v))))

;; s1 are the variable names
;; s2 are the values
(define update-binding-helper
  (lambda (var val s1 s2 return)
    (cond
      [(null? s1)         (return '( () () ))]
      [(eq? (car s1) var) (update-binding-helper var val (cdr s1) (cdr s2) (lambda (v) (return (cons (cons (car s1) (car v)) (list (cons val (cadr v)))))))]
      [else               (update-binding-helper var val (cdr s1) (cdr s2) (lambda (v) (return (cons (cons (car s1) (car v)) (list (cons (car s2) (cadr v)))))))])))

    
    




