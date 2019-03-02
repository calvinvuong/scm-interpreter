;; Imran Hossain
;; Calvin Vuong
;; Ben Young

#lang racket
(require "simpleParser.rkt")

(define initialState '((() ())))

;; calls interpret-tree on parsed code
;; r is the continuation for return
(define interpret
  (lambda (filename)
    (translate-boolean
      (call/cc
       (lambda (r) ; continuation for return
         (M-state (parser filename) initialState r))))))


;;turns #f and #t into 'false and 'true for final return
(define translate-boolean
  (lambda (x)
    (cond
      [(eq? x #f) 'false]
      [(eq? x #t) 'true]
      [else x])))

;;returns length of lis
(define list-length-cps
  (lambda (lis return)
    (cond
      [(null? lis) (return 0)]
      [else (list-length-cps (cdr lis) (lambda (v) (return (+ 1 v))))])))

(define list-length
  (lambda (lis)
    (list-length-cps lis (lambda (v) v))))

;; returns the value of variable in current state
(define get-var-value
  (lambda (state var)
    (cond
      [(eq? (find-box var state) 'none)            (error 'undeclared "Variable not declared.")]
      [(eq? (unbox (find-box var state)) 'null)    (error 'undeclared "Using before assigning.")]
      [else                                        (unbox (find-box var state))])))

;;returns numeric or boolean value of expression
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
      [(eq? (get-operator expr) 'return) (return (M-value (exp1 expr) state))]
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

;;calls M-value-cps
(define M-value
  (lambda (expr state) 
    (M-value-cps expr state (lambda (v) v)))) 
    

;;returns whether an expression is true or false
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
      
;;calls M-boolean-cps
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
        (cons (cons (cons (car binding) (caar state)) (list (cons (box (cadr binding)) (cadar state)))) (cdr state)))))

;; s1 are the list of variable names
;; s2 are the list of values
;; returns the state
(define update-binding
  (lambda (var val state)
    (cond
      [(eq? (find-box var state) 'none)            (error 'undeclared "Variable not declared.")]
      [(eq? (unbox (find-box var state)) 'null)    (error 'undeclared "Using before assigning.")]
      [else                                        (begin (set-box! (find-box var state) val)
                                                          state)])))

;; find
;; returns the box of the value of variable v
;; returns 'none if the value is not found
(define find-box
  (lambda (var state)
    (call/cc
     (lambda (k)
         ;(display state)
       (find-box-break var state k)))))

(define find-box-break
  (lambda (var state break)
    (cond
      [(null? state)                                                           'none]
      [(eq? (find-box-layer-break var (caar state) (cadar state) break) 'none)
       (find-box-break var (cdr state) break)]
      [else 
        (find-box-layer-break var (caar state) (cadar state) break)])))

; s1 is the list of vars
; s2 is the list of values
; finds in the top layer only                 
(define find-box-layer-break
  (lambda (var s1 s2 break)
    (cond
      [(null? s1)           'none] ; variable not found
      [(eq? (car s1) var)   (break (car s2))]
      [else                 (find-box-layer-break var (cdr s1) (cdr s2) break)])))

;;calls one of many M-state-** functions depending on nature of input
(define M-state
  (lambda (expr state break-return)
    (cond
      [(null? expr)                         state]
      [(eq? (get-keyword expr) 'var)        (M-state (cdr expr) 
                                                     (M-state-declare (car expr) 
                                                                      state)
                                                     break-return)]
      [(eq? (get-keyword expr) '=)          (M-state (cdr expr)
                                                     (M-state-assign (car expr) 
                                                                     state)
                                                     break-return)]
      [(eq? (get-keyword expr) 'return)     (break-return (M-state-return (car expr) state))]
      [(eq? (get-keyword expr) 'if)         (M-state (cdr expr) 
                                                     (M-state-if (car expr) 
                                                                 state
                                                                 break-return)
                                                      break-return)]
      [(eq? (get-keyword expr) 'while)      (M-state (cdr expr) 
                                                     (M-state-while (car expr) 
                                                                    state
                                                                    break-return)
                                                      break-return)]
      [(eq? (get-keyword expr) 'begin)      (M-state (cdr expr) 
                                                     (M-state-block (car expr) 
                                                                    state
                                                                    break-return)
                                                      break-return)]
      [else                                 state])))

;;helper for var-declared: checked if atom is in list
(define contains
  (lambda (a lis)
    (cond
      [(null? lis) #f]
      [(eq? a (car lis)) #t]
      [else (contains a (cdr lis))]))) 

;;checks to see if var is arleady declared in this state
;;For use in M-state-declare
(define var-declared
  (lambda (var state)
    (contains var (car state)))) 

;;Updates state for a variable declaration
(define M-state-declare
  (lambda (expr state)
    (cond
      [(var-declared (declare-var expr) state)     (error 'error "Variable already declared!")]
      [(eq? (list-length expr) 3)                  (add-to-state (cons (declare-var expr) 
                                                                       (list (M-value (caddr expr) state))) 
                                                                 state)]
      [(not (eq? (list-length expr) 2))            (error 'error "Invalid declare expression.")]
      [else                                        (add-to-state (cons (declare-var expr) '(null))
                                                                 state)])))

;; updates the state in variable assignment
(define M-state-assign
  (lambda (expr state)
    (if (eq? (list-length expr) 3)
        (update-binding (exp1 expr) 
                        (M-value (exp2 expr) state) state)
        (error 'error "Invalid assign."))))

;; return does not update state, so just pass control to M-value
;; checks if valid length
(define M-state-return
  (lambda (expr state)
    (if (eq? (list-length expr) 2)
        (M-value expr state)
        (error 'error "Invalid return."))))

(define conditional 
  (lambda (expr) 
      (list (cadr expr))))

(define body 
  (lambda (expr) 
      (list (caddr expr))))

(define rest-if cdddr) ; else if statements
;; update state if function
;; handles side effects
(define M-state-if
  (lambda (expr state break-return)
    (cond
      [(eq? (M-boolean (car (conditional expr)) state) #t) (M-state (body expr) 
                                                                    (M-state (conditional expr) state break-return)
                                                                    break-return)]
      [(> (list-length expr) 3)                      (M-state (rest-if expr) 
                                                              (M-state (conditional expr) state break-return) break-return)]
      [else                                          (M-state (conditional expr) 
                                                              state break-return)])))


;; update state while loop
(define M-state-while
  (lambda (expr state break-return)
    (if (eq? (M-boolean (car (conditional expr)) state) #t)
        (M-state-while expr 
                       (M-state (body expr) 
                                (M-state (conditional expr) state break-return) break-return) break-return)
        (M-state (conditional expr) state break-return))))
 
(define block-body cdr)
;; state with blocks - first add a new layer, then remove it
(define M-state-block
  (lambda (expr state break-return)
    (remove-layer (M-state (block-body expr)
                           (push-layer state)
                           break-return))))
  
;; Adds a new layer to top of state
(define push-layer
  (lambda (state)
    (cons empty-layer state)))

(define remove-layer cdr)


; ----- Macros -----
(define empty-layer '(()()))

(define get-operator car)
(define exp1 cadr)
(define exp2 caddr)

(define get-keyword caar)

; M-state-declare
(define declare-var cadr)

(define (atom? x) (not (or (pair? x) (null? x))))

(interpret "testcode")

