;; Imran Hossain
;; Calvin Vuong
;; Ben Young

#lang racket
(require "simpleParser.rkt")
(parser "testcode")

(define initialState '(() ()))

(define interpret
  (lambda (filename)
    (interpret-tree (parser filename) initialState)))

(define interpret-tree
  (lambda (tree state)
    (cond
      [(null? tree) (translate-boolean (get-var-value state 'return))]
      [else (interpret-tree (cdr tree) (M-state (car tree) state))])))

(define translate-boolean
  (lambda (x)
    (cond
      [(eq? x #f) 'false]
      [(eq? x #t) 'true]
      [else x])))

(define list-length-cps
  (lambda (lis return)
    (cond
      [(null? lis) (return 0)]
      [else (list-length-cps (cdr lis) (lambda (v) (return (+ 1 v))))])))

(define list-length
  (lambda (lis)
    (list-length-cps lis (lambda (v) v))))

(define get-var-value
  (lambda (state var)
    (cond
      [(null? (car state))                                    (error 'undeclared "Variable not declared.")]
      [(and (eq? (caar state) var) (eq? (caadr state) 'null)) (error 'error "Using before assigning.")]
      [(eq? (caar state) var)                                 (caadr state)]
      [else                                                   (get-var-value (cons (cdar state) (list (cdr (car (cdr state))))) var)])))

(define M-value-cps
  (lambda (expr state return)
    (cond 
      [(null? expr)                     (error 'undefined "Empty expression")]
      [(number? expr)                   (return expr)]
      ;; if expr isn't a pair and isn't a number, it's a variable
      ;; so look it up in the state
      [(eq? expr 'false)                (return #f)]
      [(eq? expr 'true)                 (return #t)]
      [(not (list? expr))               (return (get-var-value state expr))]  
      ;; if next exprression's length is 2, it's unary -
      [(and (eq? (list-length expr) 2)
            (eq? (get-operator expr) '-))
       (M-value-cps (exp1 expr) 
                    state 
                    (lambda (v) (return (* -1 v))))]
      [(eq? (get-operator expr) 'return) (return (add-to-state (cons 'return (list (M-value (exp1 expr) state))) state))]
      [(eq? (list-length expr) 2)        (error 'undefined "Incorrect number of arguments")]
      [(not (eq? (list-length expr) 3))  (error 'undefined "Incorrect number of arguments")]
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
                                              (lambda (v2) (return (remainder v1 v2))))))]
      [else                              (return (M-boolean expr state))])))

(define M-value
  (lambda (expr state) 
    (M-value-cps expr state (lambda (v) v)))) 
    

(define M-boolean-cps
  (lambda (expr state return)
    (cond
      [(null? expr)                     (error 'undefined "Incorrect number of arguments")]
      [(eq? expr 'true)                 (return #t)]
      [(eq? expr 'false)                (return #f)]
      ;;otherwise, this is a variable
      [(not (list? expr))               (return (get-var-value state expr))] 
      [(eq? (get-operator expr) '==)    (return (eq? (M-value (exp1 expr) state) (M-value (exp2 expr) state)))]
      [(eq? (get-operator expr) '!=)    (return (not (eq? (M-value (exp1 expr) state) (M-value (exp2 expr) state))))]
      [(eq? (get-operator expr) '<)     (return (< (M-value (exp1 expr) state) (M-value (exp2 expr) state)))]
      [(eq? (get-operator expr) '>)     (return (> (M-value (exp1 expr) state) (M-value (exp2 expr) state)))]
      [(eq? (get-operator expr) '<=)    (return (<= (M-value (exp1 expr) state) (M-value (exp2 expr) state)))]
      [(eq? (get-operator expr) '>=)    (return (>= (M-value (exp1 expr) state) (M-value (exp2 expr) state)))]
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
      [(eq? (car s1) var) (remove-from-state-cps var
                                                 (cdr s1)
                                                 (cdr s2)
                                                 (lambda (v) (return (cons (car v) (list (cadr v))))))]
      [else               (remove-from-state-cps var
                                                 (cdr s1)
                                                 (cdr s2)
                                                 (lambda (v) (return (cons
                                                                      (cons (car s1) (car v))
                                                                      (list (cons (car s2) (cadr v)))))))])))

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
      [(eq? (car s1) var) (update-binding-cps var
                                              val
                                              (cdr s1)
                                              (cdr s2)
                                              (lambda (v) (return (cons
                                                                   (cons (car s1) (car v))
                                                                   (list (cons val (cadr v)))))))]
      [else               (update-binding-cps var
                                              val
                                              (cdr s1)
                                              (cdr s2)
                                              (lambda (v) (return (cons
                                                                   (cons (car s1) (car v))
                                                                   (list (cons (car s2) (cadr v)))))))])))

(define M-state
  (lambda (expr state)
    (cond
      [(null? expr)                         (error 'error "Empty expression.")]
      [(eq? (get-keyword expr) 'var)        (M-state-declare expr state)]
      [(eq? (get-keyword expr) '=)          (M-state-assign expr state)]
      [(eq? (get-keyword expr) 'return)     (M-state-return expr state)]
      [(eq? (get-keyword expr) 'if)         (M-state-if expr state)]
      [(eq? (get-keyword expr) 'while)      (M-state-while expr state)]
      [else                                 state])))

(define M-state-declare
  (lambda (expr state)
    (cond
      [(eq? (list-length expr) 3)           (add-to-state (cons (declare-var expr) (list (M-value (caddr expr) state))) state)]
      [(not (eq? (list-length expr) 2))     (error 'error "Invalid declare expression.")]
      [else                                 (add-to-state (cons (declare-var expr) '(null)) state)])))

;; updates the state in variable assignment
(define M-state-assign
  (lambda (expr state)
    (if (eq? (list-length expr) 3)
        (update-binding (exp1 expr) (M-value (exp2 expr) state) state)
        (error 'error "Invalid assign."))))

;; return does not update state, so just pass control to M-value
;; checks if valid length
(define M-state-return
  (lambda (expr state)
    (if (eq? (list-length expr) 2)
        (M-value expr state)
        (error 'error "Invalid return."))))

(define conditional cadr)
(define body caddr)
(define rest-if cadddr) ; else if statements
;; update state if function
;; handles side effects
(define M-state-if
  (lambda (expr state)
    (cond
      [(eq? (M-boolean (conditional expr) state) #t) (M-state (body expr) (M-state (conditional expr) state))]
      [(> (list-length expr) 3)                      (M-state (rest-if expr) (M-state (conditional expr) state))]
      [else                                          (M-state (conditional expr) state)])))

;;    (if (eq? (M-boolean (conditional expr) state) #t)
;;        (M-state (body expr) (M-state (conditional expr) state)) ; return updated state from executing body and evaluating conditional
;;        (M-state (conditional expr) state)))) ; return updated state from evaluating conditional
        

;; update state while loop
;; handles side effects
(define M-state-while
  (lambda (expr state)
    (if (eq? (M-boolean (conditional expr) state) #t)
        (M-state-while expr (M-state (body expr) (M-state (conditional expr) state)))
        (M-state (conditional expr) state))))
 

; ----- Macros -----

(define get-operator car)
(define exp1 cadr)
(define exp2 caddr)

(define get-keyword car)

; M-state-declare
(define declare-var cadr)

(define (atom? x) (not (or (pair? x) (null? x))))

