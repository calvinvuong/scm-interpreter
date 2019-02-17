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

(define (atom? x) (not (or (pair? x) (null? x))))

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

(M-boolean '(|| (== (% a 3) 0) (> (+ a b) 12)) '((a c b) (1 2 3)))
