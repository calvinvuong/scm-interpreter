;; Imran Hossain
;; Calvin Vuong
;; Ben Young

#lang racket
(require "simpleParser.rkt")

(define initialState '((() ())))

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
                  return
                  (lambda (v) (error 'error "Improper break placement."))
                  (lambda (v) (error 'error "Improper continue placement"))
                  (lambda (v) (error 'invalid_throw "Not in try/catch. Cannot throw exception"))))))))

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
      [(eq? (unbox (find-box var state)) 'null)    (error 'uninitialized "Using before assigning.")]
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
  (lambda (expr state break-return break continue throw)
    (cond
      [(null? expr)                         state]
      ;; to handle if (true) or while (true)
      [(eq? (car expr) 'true)               state]
      [(eq? (car expr) 'false)              state]
      [(eq? (get-keyword expr) 'var)        (M-state (cdr expr) 
                                                     (M-state-declare (car expr) 
                                                                      state)
                                                     break-return
                                                     break
                                                     continue
                                                     throw)]
      
      [(eq? (get-keyword expr) '=)          (M-state (cdr expr)
                                                     (M-state-assign (car expr) 
                                                                     state)
                                                     break-return
                                                     break
                                                     continue
                                                     throw)]
      ; a "goto" construct return
      [(eq? (get-keyword expr) 'return)     (break-return (M-state-return (car expr) state))]
      ; a "goto" construct break
      [(eq? (get-keyword expr) 'break)      (break (remove-layer state))]
      ; a "goto" construct continue
      [(eq? (get-keyword expr) 'continue)   (continue state)]
      [(eq? (get-keyword expr) 'throw)      (throw (list (get-throw-value expr state) state))]
      
      [(eq? (get-keyword expr) 'if)         (M-state (cdr expr) 
                                                     (M-state-if (car expr) 
                                                                 state
                                                                 break-return
                                                                 break
                                                                 continue
                                                                 throw)
                                                      break-return
                                                      break
                                                      continue
                                                      throw)]
   
      [(eq? (get-keyword expr) 'while)      (M-state (cdr expr) 
                                                      (call/cc
                                                       (lambda (br) ; br parameter: break continuation
                                                         (M-state-while (car expr) 
                                                                        state
                                                                        break-return
                                                                        br
                                                                        continue
                                                                        throw)))
                                                      break-return
                                                      break
                                                      continue
                                                      throw)]
      
      [(eq? (get-keyword expr) 'begin)      (M-state (cdr expr) 
                                                     (M-state-block (car expr) 
                                                                    state
                                                                    break-return
                                                                    break
                                                                    continue
                                                                    throw)
                                                      break-return
                                                      break
                                                      continue
                                                      throw)]

      [(eq? (get-keyword expr) 'try)      (M-state (cdr expr) 
                                                     (M-state-trycatchblock 
                                                       (car expr) 
                                                       state
                                                       break-return
                                                       break
                                                       continue
                                                       throw)
                                                     break-return
                                                     break
                                                     continue
                                                     throw)]
      [else                                 state])))


;;checks to see if var is arleady declared in this state
;;For use in M-state-declare
(define var-declared
  (lambda (var state)
    (not (eq? (find-box var state) 'none))))

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
(define M-state-if
  (lambda (expr state break-return break continue throw)
    (cond
      [(eq? (M-boolean (car (conditional expr)) state) #t) (M-state (body expr) 
                                                                    (M-state (conditional expr)
                                                                             state 
                                                                             break-return 
                                                                             break
                                                                             continue
                                                                             throw)
                                                                    break-return
                                                                    break
                                                                    continue
                                                                    throw)]
      [(> (list-length expr) 3)                      (M-state (rest-if expr) 
                                                              (M-state (conditional expr) state break-return break continue throw) break-return break continue throw)]
      [else                                          (M-state (conditional expr) 
                                                              state
                                                              break-return
                                                              break
                                                              continue
                                                              throw)])))

(define M-state-trycatchblock
  (lambda (expr state break-return break continue throw)
    (if (and (null? (catch-block expr)) (null? (finally-block expr)))
      (error 'error "Must have either catch or finally block")
      (M-state-catch 
        expr
            (M-state-try 
              (try-block expr) 
              state 
              (lambda (return-statement) (M-state-finally (finally-expression expr) 
                                                          state break-return 
                                                          break continue throw 
                                                          break-return 
                                                          (list 'break-return return-statement)))
              (lambda (v) (M-state-finally (finally-expression expr) 
                                          (remove-layer state) break-return 
                                          break continue throw 
                                          break 'break))
              (lambda (v) (M-state-finally (finally-expression expr) 
                                          (remove-layer state) break-return 
                                          break continue throw 
                                          continue 'continue))
              throw)
        (lambda (return-statement) (M-state-finally (finally-expression expr) 
                                                    state break-return 
                                                    break continue throw 
                                                    break-return 
                                                    (list 'break-return return-statement)))
        (lambda (v) (M-state-finally (finally-expression expr) 
                                     (remove-layer state) break-return 
                                     break continue throw 
                                     break 'break))
        (lambda (v) (M-state-finally (finally-expression expr) 
                                     (remove-layer state) break-return 
                                     break continue throw 
                                     continue 'continue))
        (lambda (throw-statement) (M-state-finally (finally-expression expr) 
                                                   state break-return 
                                                   break continue throw 
                                                   throw (list 'throw throw-statement)))))))

(define M-state-try
  (lambda (expr state break-return break continue throw)
    (call/cc
     (lambda (new-throw)
       (M-state-trycatchexpr (cons 'begin expr) state break-return break continue new-throw)))))

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
  (lambda (expr state break-return break continue throw)
    (cond
      ;; FIXME
      [(null? (catch-block expr)) (M-state-finally (finally-expression expr)
                                                   state break-return 
                                                   break continue throw
                                                   (lambda (v) v) 'null)]
      ;;If we threw an exception, state will be input from the call/cc
      ;;in which case its car is the first argument to catch, which isn't a list
      ;;so this is the case where we didn't throw an exception
      [(list? (car state)) (M-state-finally (finally-expression expr)
                                            state
                                            break-return break continue throw
                                            (lambda (v) v) 'null)]
      ;;if we did throw an exception
      [else (M-state-finally (finally-expression expr)
                             (M-state-catch-recurse (catch-expression expr)
                                                    (add-to-state (cons (get-caught-var expr)
                                                                        (list (car state)))
                                                                  (remove-layer (cadr state)))
                                                    break-return break continue throw)
                             break-return break continue throw
                             (lambda (v) v) 'null)])))

(define M-state-catch-recurse
  (lambda (expr state break-return break continue throw)
    (M-state-trycatchexpr (cons 'begin expr) state break-return break continue throw)))

(define M-state-finally-recurse
  (lambda (expr state break-return break continue throw do-at-end do-at-end-name)
    (cond 
      [(not (null? expr)) (M-state-finally-recurse (cdr expr) 
                                                   (M-state (list (car expr))
                                                            state break-return
                                                            break continue throw)
                                                   break-return break continue 
                                                   throw do-at-end do-at-end-name)]
      [(eq? do-at-end-name 'null) (do-at-end state)]
      ;;if do-at-end-name is a list, then this is a return
      [(and (list? do-at-end-name)
            (eq? (car do-at-end-name) 'break-return)) 
       (do-at-end (M-state-return (list 'return (cadr do-at-end-name)) state))]
      [(and (list? do-at-end-name)
            (eq? (car do-at-end-name) 'throw))
       (do-at-end (cadr do-at-end-name))]
      [(eq? do-at-end-name 'continue) (do-at-end state)]
      [(eq? do-at-end-name 'break) (do-at-end state)])))

(define M-state-finally
  (lambda (expr state break-return break continue throw do-at-end do-at-end-name)
    (remove-layer (M-state-finally-recurse
                    expr (push-layer state) break-return break continue throw do-at-end do-at-end-name))))
         
;;Like M-state-block, but we don't overwrite the continue continuation.
;;Used for what would be the 'block' inside try and catch
(define M-state-trycatchexpr
  (lambda (expr state break-return break continue throw)
         (remove-layer
               (M-state (block-body expr)
                        (push-layer state)
                        break-return
                        break
                        continue
                        throw))))

(define try-block cadr)
(define catch-block caddr)
(define finally-block cadddr)

;; update state while loop
(define M-state-while
  (lambda (expr state break-return break continue throw)
    (if (eq? (M-boolean (car (conditional expr)) state) #t)
        (M-state-while expr 
                       (M-state (body expr) 
                                (M-state (conditional expr) state break-return break continue throw)
                                break-return
                                break
                                continue
                                throw)
                       break-return
                       break
                       continue
                       throw)
        (M-state (conditional expr) state break-return break continue throw))))
 
(define block-body cdr)

;; state with blocks - first add a new layer, then remove it
(define M-state-block
  (lambda (expr state break-return break continue throw)
         (remove-layer
           (call/cc 
             (lambda (cont) ; continuation for continue
               (M-state (block-body expr)
                        (push-layer state)
                        break-return
                        break
                        cont
                        throw))))))

  
  
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
  (lambda (expr state)
    (M-value (cadar expr) state)))

; M-state-declare
(define declare-var cadr)

(define (atom? x) (not (or (pair? x) (null? x))))

(interpret "testcode")
