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
    (remove-from-state-cps var (car state) (cadr state) (lambda (v) v))))

;; s1 are the variable names
;; s2 are the values
(define remove-from-state-cps
  (lambda (var s1 s2 return)
    (cond
      [(null? s1)         (return '( () () ))]
      [(eq? (car s1) var) (remove-from-state-cps var (cdr s1) (cdr s2) (lambda (v) (return (cons (car v) (list (cadr v))))))]
      [else               (remove-from-state-cps var (cdr s1) (cdr s2) (lambda (v) (return (cons (cons (car s1) (car v)) (list (cons (car s2) (cadr v)))))))])))

;; update a variable value in state
;; wrapper function
(define update-binding
  (lambda (var val state)
    (update-binding-cps var val (car state) (cadr state) (lambda (v) v))))

;; s1 are the variable names
;; s2 are the values
(define update-binding-cps
  (lambda (var val s1 s2 return)
    (cond
      [(null? s1)         (return '( () () ))]
      [(eq? (car s1) var) (update-binding-cps var val (cdr s1) (cdr s2) (lambda (v) (return (cons (cons (car s1) (car v)) (list (cons val (cadr v)))))))]
      [else               (update-binding-cps var val (cdr s1) (cdr s2) (lambda (v) (return (cons (cons (car s1) (car v)) (list (cons (car s2) (cadr v)))))))])))

(define M-state
  (lambda (expr state)
    (cond
      [(null? expr)                         (error 'error "Empty expression.")]
      [(eq? (get-keyword expr) 'var)        (M-state-declare expr state)]
      [(eq? (get-keyword expr) '=)          (M-state-assign expr state)]
      [(eq? (get-keyword expr) 'return)     (M-state-return expr state)]
      [(eq? (get-keyword expr) 'if)         (M-state-if expr state)]
      [(eq? (get-keyword expr) 'while)      (M-state-while expr state)]
      #| [(and (eq? (get-operator car) 'var) |#
      #|       (eq? (list-length expr) 3)) |#
      #|  (add-to-state (exp1 expr) (exp2 expr))] |#
      #| [(and (eq? (get-operator car) '=) |#
      #|       (eq? (list-length expr) 3)) |#
      #|  (update-binding (exp1 expr) (exp2 expr))] |#
      #| [(and (eq? (get-operator expr) 'return) |#
      #|       (eq? (list-length expr) 2)) |#
       )))

#| (define M-state-declare |#
#|   (lambda (expr state) |#
#|     (M-state-declare-cps expr state (lambda (v) v)))) |#

(define M-state-declare
  (lambda (expr state return)
    (cond
      [(not (eq? (list-length expr) 2))     (error 'error "Invalid declare expression.")]
      [else                                 (add-to-state (declare-var expr) 'null)])))

;; updates the state in variable assignment
(define M-state-assign
  (lambda (expr state)
    (if (eq? (list-length-cps expr (lambda (v) v)) 3)
        (update-binding (exp1 expr) (M-value (exp2 expr) state) state)
        (error 'error "Invalid assign."))))

;; return does not update state, so just pass control to M-value
;; checks if valid length
(define M-state-return
  (lambda (expr state)
    (if (eq? (list-length-cps expr (lambda (v) v)) 2)
        (M-value (expr state))
        (error 'error "Invalid return."))))

(define conditional cadr)
(define body caddr)
;; update state if function
;; handles side effects
(define M-state-if
  (lambda (expr state)
    (if (eq? (M-boolean (conditional expr) state) #t)
        (M-state (body expr) (M-state (conditional expr) state)) ; return updated state from executing body and evaluating conditional
        (M-state (conditional expr) state)))) ; return updated state from evaluating conditional
        

;; update state while loop
;; handles side effects
(define M-state-while
  (lambda (expr state)
    (if (eq? (M-boolean (conditonal expr) state) #t)
        (M-state-while-cps expr (M-state (body expr) (M-state (conditional expr) state)))
        (M-state (conditional expr) state))))
    

; ----- Macros -----

(define get-operator car)
(define exp1 cadr)
(define exp2 caddr)

; M-state
(define get-keyword car)

; M-state-declare
(define declare-var car)
