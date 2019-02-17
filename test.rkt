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

(define get-var-value
  (lambda (state var)
    (cond
      [(null? (car state)) 'null]
      [(eq? (caar state) var) (caadr state)]
      [else (get-var-value (cons (cdar state) (list (cdr (car (cdr state))))) var)])))

(define M-value-cps
  (lambda (expr state return)
    (cond 
      [(null? expr) (error 'undefined "ya know jimbo that's not a valid expression")]
      [(number? expr) (return expr)]
      ;; if expr isn't a pair and isn't a number, it's a variable
      ;; so look it up in the state
      [(not (list? expr)) (return (get-var-value state expr))]  
      ;; if next exprression's length is 2, it's unary -
      [(and (eq? (list-length expr) 2)
            (eq? (get-operator expr) '-))
       (M-value-cps (exp1 expr) 
                    state 
                    (lambda (v) (return (* -1 v))))]
      [(eq? (list-length expr) 2) (error 'undefined "ya know jimbo that's not a valid exprression")]
      [(not (eq? (list-length expr) 3)) (error 'undefined "ya know jimbo that's not a valid exprression")]
      [(eq? (get-operator expr) '+) 
       (M-value-cps (exp1 expr) 
                    state
                    (lambda (v1) (M-value-cps (exp2 expr) 
                                              state
                                              (lambda (v2) (return (+ v1 v2))))))]
      [(eq? (get-operator expr) '-) 
       (M-value-cps (exp1 expr) 
                    state
                    (lambda (v1) (M-value-cps (exp2 expr) 
                                              state
                                              (lambda (v2) (return (- v1 v2))))))]
      [(eq? (get-operator expr) '*) 
       (M-value-cps (exp1 expr) 
                    state
                    (lambda (v1) (M-value-cps (exp2 expr) 
                                              state
                                              (lambda (v2) (return (* v1 v2))))))]
      [(eq? (get-operator expr) '/) 
       (M-value-cps (exp1 expr) 
                    state
                    (lambda (v1) (M-value-cps (exp2 expr) 
                                              state
                                              (lambda (v2) (return (quotient v1 v2))))))]
      [(eq? (get-operator expr) '%) 
       (M-value-cps (exp1 expr) 
                    state
                    (lambda (v1) (M-value-cps (exp2 expr) 
                                              state
                                              (lambda (v2) (return (remainder v1 v2))))))])))

(define M-value
  (lambda (expr state) 
    (M-value-cps expr state (lambda (v) v)))) 
    

(define M-boolean-cps
  (lambda (expr state return)
    (cond
      [(null? expr) (error 'undefined "ya know jimbo that's not a valid expression")]
      [(atom? expr) (error 'undefined "ya know jimbo that's not a valid expression")]
      [(eq? (get-operator expr) '==) 
       (return (eq? (M-value (exp1 expr) state) (M-value (exp2 expr) state)))]
      [(eq? (get-operator expr) '!=) 
       (return (not (eq? (M-value (exp1 expr) state) (M-value (exp2 expr) state))))]
      [(eq? (get-operator expr) '<) 
       (return (< (M-value (exp1 expr) state) (M-value (exp2 expr) state)))]
      [(eq? (get-operator expr) '>) 
       (return (> (M-value (exp1 expr) state) (M-value (exp2 expr) state)))]
      [(eq? (get-operator expr) '<=) 
       (return (<= (M-value (exp1 expr) state) (M-value (exp2 expr) state)))]
      [(eq? (get-operator expr) '>=) 
       (return (>= (M-value (exp1 expr) state) (M-value (exp2 expr) state)))]
      [(eq? (get-operator expr) '&&) 
       (M-boolean-cps (exp1 expr) 
                      state
                      (lambda (v1) (M-boolean-cps (exp2 expr) 
                                                  state
                                                  (lambda (v2) (return (and v1 v2))))))]
      [(eq? (get-operator expr) '||) 
       (M-boolean-cps (exp1 expr) 
                      state
                      (lambda (v1) (M-boolean-cps (exp2 expr) 
                                                  state
                                                  (lambda (v2) (return (or v1 v2))))))]
      [(eq? (get-operator expr) '!) 
       (M-boolean-cps (exp1 expr) 
                      state
                      (lambda (v) (return (not v))))])))
      

(define M-boolean
  (lambda (expr state)
    (M-boolean-cps expr state (lambda (v) v))))

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

(define get-keyword car)

; M-state-declare
(define declare-var car)

(define (atom? x) (not (or (pair? x) (null? x))))


(M-boolean '(|| (== (% a 3) 0) (> (+ a b) 12)) '((a c b) (1 2 3)))
