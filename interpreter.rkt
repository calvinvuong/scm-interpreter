;; Imran Hossain
;; Calvin Vuong
;; Ben Young

#lang racket
(require "simpleParser.rkt")

(define initialState '((() ())))
(define initialContinuations 
  (lambda (return)
    (make-immutable-hash
      (list (cons 'return return)
            (cons 'break 
                  (lambda (v) 
                    (error 'error "Improper break placement.")))
            (cons 'continue 
                  (lambda (v) 
                    (error 'error "Improper continue placement.")))
            (cons 'throw 
                  (lambda (v) 
                    (error 'invalid_throw "Not in try/catch. Cannot throw exception")))))))

;; calls interpret-tree on parsed code
;; r is the continuation for return
; lambda (v) v is just the "default" continuation for break and continue
(define interpret
  (lambda (filename)
    (translate-boolean
      (call/cc
       (lambda (return) ; continuation for return
         ;;(M-state expr state return break continue throw)
         ;;initially break, continue, and throw should give errors
         ;;because we aren't in a structure where we can call them
         (M-state (parser filename)
                  initialState
                  (initialContinuations return)))))))

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
      [else (list-length-cps (cdr lis) (lambda (v) 
                                         (return (+ 1 v))))])))

(define list-length
  (lambda (lis)
    (list-length-cps lis (lambda (v) v))))

;; returns the value of variable in current state
(define get-var-value
  (lambda (state var)
    (cond
      [(eq? (find-box var state) 'none)            (error 'undeclared "Variable not declared.")]
      [(eq? (unbox (find-box var state)) 'null)    (error 'uninitialized "Using before assigning.")]
      [else                                        (unbox (find-box var state))])))


;;returns numeric or boolean value of expression
(define M-value-cps
  (lambda (expr state cps-return continuations)
    (cond 
      [(null? expr)                     (error 'undefined "Empty expression")]
      [(number? expr)                   (cps-return expr)]
      ;; if expr isn't a pair and isn't a number, it's a variable
      ;; so look it up in the state
      [(eq? expr 'false)                (cps-return #f)]
      [(eq? expr 'true)                 (cps-return #t)]
      [(not (list? expr))               (cps-return (get-var-value state expr))]  
      ;; if next exprression's length is 2, it's unary -
      [(and (eq? (list-length expr) 2)
            (eq? (get-operator expr) '-))
       (M-value-cps (exp1 expr) 
                    state 
                    (lambda (v) 
                      (cps-return (* -1 v)))
                    continuations)]
      ;;function
      ;;[(eq? (get-operator expr) 'funcall) (cps-return (M-value-function expr state cps-return))]
      [(eq? (get-operator expr) 'return) (cps-return (M-value (exp1 expr) state continuations))]
      [(eq? (list-length expr) 2)        (error 'undefined "Incorrect number of arguments")]
      [(not (eq? (list-length expr) 3))  (error 'undefined "Incorrect number of arguments")]
      [(eq? (get-operator expr) '+) (M-value-math-operator expr state + cps-return continuations)]
      [(eq? (get-operator expr) '-) (M-value-math-operator expr state - cps-return continuations)]
      [(eq? (get-operator expr) '*) (M-value-math-operator expr state * cps-return continuations)]
      [(eq? (get-operator expr) '/) (M-value-math-operator expr state quotient cps-return continuations)]
      [(eq? (get-operator expr) '%) (M-value-math-operator expr state remainder cps-return continuations)]
      [else (cps-return (M-boolean expr state continuations))])))

;;calls M-value-cps
(define M-value
  (lambda (expr state continuations) 
    (M-value-cps expr state (lambda (v) v) continuations)))

;;cps-returns the value of a function
;;pass in a new cps-return
;;(define M-value-function 
 ;; (lambda (expr enviro cps-return)

    
;;abstraction for when M-value-cps is given an arithmetic operation
(define M-value-math-operator 
  (lambda (expr state operation cps-return continuations)
    (M-value-cps (exp1 expr) 
                 state
                 (lambda (v1) 
                   (M-value-cps (exp2 expr) 
                                state
                                (lambda (v2) 
                                  (cps-return (operation v1 v2)))
                                continuations))
                 continuations)))

;;returns whether an expression is true or false
(define M-boolean-cps
  (lambda (expr state cps-return continuations)
    (cond
      [(null? expr)                     (error 'undefined "Incorrect number of arguments")]
      [(eq? expr 'true)                 (cps-return #t)]
      [(eq? expr 'false)                (cps-return #f)]
      ;;otherwise, this is a variable
      [(not (list? expr))               (cps-return (get-var-value state expr))] 
      [(eq? (get-operator expr) '==)    (M-boolean-comparator expr state cps-return eq? continuations)]
      [(eq? (get-operator expr) '!=)    (M-boolean-comparator expr 
                                                              state 
                                                              cps-return
                                                              (lambda (a b)
                                                                (not (eq? a b)))
                                                              continuations)]
      [(eq? (get-operator expr) '<)    (M-boolean-comparator expr state cps-return < continuations)]
      [(eq? (get-operator expr) '>)    (M-boolean-comparator expr state cps-return > continuations)]
      [(eq? (get-operator expr) '<=)   (M-boolean-comparator expr state cps-return <= continuations)]
      [(eq? (get-operator expr) '>=)   (M-boolean-comparator expr state cps-return >= continuations)]
      [(eq? (get-operator expr) '&&)   (M-boolean-logic-operator 
                                         expr state 
                                         (lambda (a b) 
                                           (and a b))
                                         cps-return continuations)] 
      [(eq? (get-operator expr) '||)   (M-boolean-logic-operator 
                                         expr state 
                                         (lambda (a b) 
                                           (or a b))
                                         cps-return continuations)] 
      [(eq? (get-operator expr) '!) 
       (M-boolean-cps (exp1 expr) 
                      state
                      (lambda (v) 
                        (cps-return (not v)))
                      continuations)]
      [else (error 'invalid_expression "Invalid value or boolean expression!")])))
      
;;calls M-boolean-cps
(define M-boolean
  (lambda (expr state continuations)
    (M-boolean-cps expr state (lambda (v) v) continuations)))

;;abstraction for when M-boolean-cps is given an comparator (<, >, ==, etc) operation
(define M-boolean-comparator
  (lambda (expr state cps-return operation continuations)
    (cps-return (operation (M-value (exp1 expr) state continuations)
                           (M-value (exp2 expr) state continuations)))))

;;abstraction for when M-boolean-cps is given a logic operation
(define M-boolean-logic-operator 
  (lambda (expr state operation cps-return continuations)
    (M-boolean-cps (exp1 expr) 
                    state
                    (lambda (v1) 
                      (M-boolean-cps (exp2 expr) 
                                   state
                                   (lambda (v2) 
                                     (cps-return (operation v1 v2)))))
                    continuations)))

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
      ;; FIXME
      ;;[(eq? (unbox (find-box var state)) 'null)    (error 'undeclared "Using before assigning.")]
      [else                                        (begin (set-box! (find-box var state) val)
                                                          state)])))

;; find
;; returns the box of the value of variable v
;; returns 'none if the value is not found
;; uses a break continuation to find first value only
(define find-box
  (lambda (var state)
    (call/cc
     (lambda (k)
       (find-box-break var state k)))))

(define find-box-break
  (lambda (var state break)
    (cond
      [(null? state)                                                           'none]
      ; found in first layer
      [(eq? (find-box-layer-break var (caar state) (cadar state) break) 'none)
       (find-box-break var (cdr state) break)]
      ; search deeper layers
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
  (lambda (expr state continuations)
    (cond
      [(null? expr)                         state]
      ;; to handle if (true) or while (true)
      [(eq? (car expr) 'true)               state]
      [(eq? (car expr) 'false)              state]
      [(eq? (get-keyword expr) 'var)        (M-state (cdr expr) 
                                                     (M-state-declare (car expr) 
                                                                      state
                                                                      continuations)
                                                     continuations)]
      
      [(eq? (get-keyword expr) '=)          (M-state (cdr expr)
                                                     (M-state-assign (car expr) 
                                                                     state
                                                                     continuations)
                                                     continuations)]
      ; a "goto" construct return
      [(eq? (get-keyword expr) 'return)     ((hash-ref continuations 'return)
                                               (M-state-return (car expr) 
                                                               state
                                                               continuations))]
      ; a "goto" construct break
      [(eq? (get-keyword expr) 'break)      ((hash-ref continuations 'break)
                                               (remove-layer state))]
      ; a "goto" construct continue
      [(eq? (get-keyword expr) 'continue)   ((hash-ref continuations 'continue)
                                               state)]
      [(eq? (get-keyword expr) 'throw)      ((hash-ref continuations 'throw)
                                               (list (get-throw-value 
                                                       expr state continuations) 
                                                     state))]
      
      [(eq? (get-keyword expr) 'if)         (M-state (cdr expr) 
                                                     (M-state-if (car expr) 
                                                                 state
                                                                 continuations)
                                                      continuations)]
   
      [(eq? (get-keyword expr) 'while)      (M-state (cdr expr) 
                                                      (call/cc
                                                       (lambda (br) ; br parameter: break continuation
                                                         (M-state-while (car expr) 
                                                                        state
                                                                        (hash-set 
                                                                          continuations 
                                                                          'break 
                                                                          br))))
                                                      continuations)]
      
      [(eq? (get-keyword expr) 'begin)      (M-state (cdr expr) 
                                                     (M-state-block (car expr) 
                                                                    state
                                                                    continuations)
                                                      continuations)]

      [(eq? (get-keyword expr) 'try)      (M-state (cdr expr) 
                                                     (M-state-trycatchblock 
                                                       (car expr) 
                                                       state
                                                       continuations)
                                                     continuations)]
      [else                                 state])))


;;checks to see if var is arleady declared in this state
;;For use in M-state-declare
(define var-declared
  (lambda (var state)
    (not (eq? (find-box var state) 'none))))

;;Updates state for a variable declaration
(define M-state-declare
  (lambda (expr state continuations)
    (cond
      [(var-declared (declare-var expr) state)     (error 'error "Variable already declared!")]
      [(eq? (list-length expr) 3)                  (add-to-state (cons (declare-var expr) 
                                                                       (list (M-value (caddr expr)
                                                                                      state
                                                                                      continuations))) 
                                                                 state)]
      [(not (eq? (list-length expr) 2))            (error 'error "Invalid declare expression.")]
      [else                                        (add-to-state (cons (declare-var expr) '(null))
                                                                 state)])))

;; updates the state in variable assignment
(define M-state-assign
  (lambda (expr state continuations)
    (if (eq? (list-length expr) 3)
        (update-binding (exp1 expr) 
                        (M-value (exp2 expr)
                                 state
                                 continuations)
                        state)
        (error 'error "Invalid assign."))))

;; return does not update state, so just pass control to M-value
;; checks if valid length
(define M-state-return
  (lambda (expr state continuations)
    (if (eq? (list-length expr) 2)
        (M-value expr state continuations)
        (error 'error "Invalid return."))))

(define conditional 
  (lambda (expr) 
      (list (cadr expr))))

(define body 
  (lambda (expr) 
      (list (caddr expr))))

(define rest-if cdddr) ; else if statements
;; update state if function
(define M-state-if
  (lambda (expr state continuations)
    (cond
      [(eq? (M-boolean (car (conditional expr)) 
                       state 
                       continuations)
            #t) 
       (M-state (body expr) 
                (M-state (conditional expr)
                         state 
                         continuations)
                continuations)]
      [(> (list-length expr) 3)                      
       (M-state (rest-if expr) 
                (M-state (conditional expr) state continuations)
                continuations)]
      [else (M-state (conditional expr) 
                     state
                     continuations)])))

(define M-state-trycatchblock
  (lambda (expr state continuations)
    (if (and (null? (catch-block expr)) 
             (null? (finally-block expr)))
      (error 'error "Must have either catch or finally block")
      (M-state-catch 
        expr
            (M-state-try 
              (try-block expr) 
              state 
              (hash-set*
                continuations
                'return
                (lambda (return-statement) 
                  (M-state-finally (finally-expression expr) 
                                   state 
                                   (hash-set* continuations
                                              'do-at-end 
                                              (hash-ref continuations 'return)
                                              'do-at-end-name
                                              (list 'return return-statement))))

                'break
                (lambda (v) 
                  (M-state-finally (finally-expression expr) 
                                   (remove-layer state)
                                   (hash-set* continuations
                                              'do-at-end 
                                              (hash-ref continuations 'break)
                                              'do-at-end-name
                                              'break)))
                'continue
                (lambda (v) 
                  (M-state-finally (finally-expression expr) 
                                   (remove-layer state)
                                   (hash-set* continuations
                                              'do-at-end 
                                              (hash-ref continuations 'continue)
                                              'do-at-end-name
                                              'continue)))))
        (hash-set* 
          continuations
          'return
          (lambda (return-statement) 
            (M-state-finally (finally-expression expr) 
                             state 
                             (hash-set* continuations
                                        'do-at-end 
                                        (hash-ref continuations 'return)
                                        'do-at-end-name
                                        (list 'return return-statement))))
          'break
          (lambda (v) 
            (M-state-finally (finally-expression expr) 
                             (remove-layer state)
                             (hash-set* continuations
                                        'do-at-end 
                                        (hash-ref continuations 'break)
                                        'do-at-end-name
                                        'break)))
          'continue
          (lambda (v) 
            (M-state-finally (finally-expression expr) 
                             (remove-layer state)
                             (hash-set* continuations
                                        'do-at-end 
                                        (hash-ref continuations 'continue)
                                        'do-at-end-name
                                        'continue)))
          'throw
          (lambda (throw-statement)
            (M-state-finally (finally-expression expr) 
                             (remove-layer state)
                             (hash-set* continuations
                                        'do-at-end 
                                        (hash-ref continuations 'throw)
                                        'do-at-end-name
                                        (list 'throw throw-statement)))))))))

(define M-state-try
  (lambda (expr state continuations)
    (call/cc
     (lambda (new-throw)
       (M-state-trycatchexpr (cons 'begin expr) 
                             state 
                             (hash-set continuations
                                       'throw
                                       new-throw))))))

(define get-caught-var 
  (lambda (expr) 
    (caadr (catch-block expr))))

(define catch-expression
  (lambda (expr)
    (caddr (catch-block expr))))

(define finally-expression
  (lambda (expr)
    (if (null? (finally-block expr))
      '()
      (cadr (finally-block expr)))))

(define M-state-catch
  (lambda (expr state continuations)
    (cond
      ;;if there was no catch block
      [(null? (catch-block expr)) (M-state-finally (finally-expression expr)
                                                   state
                                                   (hash-set* continuations
                                                              'do-at-end 
                                                              (lambda (v) v)
                                                              'do-at-end-name
                                                              'null))]
      ;;This is the case where we didn't throw an exception:
      ;;If we threw an exception, state will be input from the call/cc
      ;;in which case its car is the first argument to catch, which isn't a list
      ;;so this case is the same as if we didn't have a catch block
      [(list? (car state)) (M-state-finally (finally-expression expr)
                                            state
                                            (hash-set* continuations
                                                       'do-at-end 
                                                       (lambda (v) v)
                                                       'do-at-end-name
                                                       'null))]
      ;;if we did throw an exception
      [else (M-state-finally (finally-expression expr)
                             (M-state-catch-recurse (catch-expression expr)
                                                    (add-to-state (cons (get-caught-var expr)
                                                                        (list (car state)))
                                                                  (remove-layer (cadr state)))
                                                    continuations)
                             (hash-set* continuations
                                        'do-at-end
                                        (lambda (v) v)
                                        'do-at-end-name
                                        'null))])))

(define M-state-catch-recurse
  (lambda (expr state continuations)
    (M-state-trycatchexpr (cons 'begin expr) state continuations)))

(define M-state-finally-recurse
  (lambda (expr state continuations)
    (cond 
      [(not (null? expr)) (M-state-finally-recurse (cdr expr) 
                                                   (M-state (list (car expr))
                                                            state 
                                                            continuations)
                                                   continuations)]
      [(eq? (hash-ref continuations 'do-at-end-name) 'null) 
       ((hash-ref continuations 'do-at-end) state)]
      ;;if do-at-end-name is a list, then this is a return or a throw
      [(and (list? (hash-ref continuations 'do-at-end-name))
            (eq? (car (hash-ref continuations 'do-at-end-name))
                 'return))
       ((hash-ref continuations 'do-at-end) 
        (M-state-return (list 'return (cadr (hash-ref continuations 'do-at-end-name))) 
                                                            state 
                                                            continuations))]
      [(and (list? (hash-ref continuations 'do-at-end-name))
            (eq? (car (hash-ref continuations 'do-at-end-name))
                 'throw))
       ((hash-ref continuations 'do-at-end) (cadr (hash-ref continuations 'do-at-end-name)))]
      [(eq? (hash-ref continuations 'do-at-end-name) 'continue) ((hash-ref continuations 'do-at-end) state)]
      [(eq? (hash-ref continuations 'do-at-end-name) 'break) ((hash-ref continuations 'do-at-end) state)])))

(define M-state-finally
  (lambda (expr state continuations)
    (remove-layer (M-state-finally-recurse
                    expr (push-layer state) continuations))))
         
;;Like M-state-block, but we don't overwrite the continue continuation.
;;Used for what would be the 'block' inside try and catch
(define M-state-trycatchexpr
  (lambda (expr state continuations)
         (remove-layer
               (M-state (block-body expr)
                        (push-layer state)
                        continuations))))

(define try-block cadr)
(define catch-block caddr)
(define finally-block cadddr)

;; update state while loop
(define M-state-while
  (lambda (expr state continuations)
    (if (eq? (M-boolean (car (conditional expr)) state continuations) #t)
        (M-state-while expr 
                       (M-state (body expr) 
                                (M-state (conditional expr) state continuations)
                                continuations)
                       continuations)
        (M-state (conditional expr) state continuations))))
 
(define block-body cdr)

;; state with blocks - first add a new layer, then remove it
(define M-state-block
  (lambda (expr state continuations)
         (remove-layer
           (call/cc 
             (lambda (cont) ; continuation for continue
               (M-state (block-body expr)
                        (push-layer state)
                        (hash-set continuations
                                  'continue
                                  cont)))))))
  
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

(define get-throw-value 
  (lambda (expr state continuations)
    (M-value (cadar expr) state continuations)))

; M-state-declare
(define declare-var cadr)

(define (atom? x) (not (or (pair? x) (null? x))))

(interpret "testcode")
