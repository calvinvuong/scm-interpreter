#lang racket

(require rackunit "interpreter.rkt")
(require rackunit/text-ui)

;; Run suite of tests for part 3
(define part3-tests
  (test-suite
    "Tests for part 3"

    (test-equal?    "A main with code inside"
                    (interpret "tests/test1")   10)
    (test-equal?    "A function that uses global variables"
                    (interpret "tests/test2")   14)
    (test-equal?    "A function that changes global variables"
                    (interpret "tests/test3")   45)
    (test-equal?    "A recursive function"
                    (interpret "tests/test4")   55)
    (test-equal?    "Functions with multiple params that hide global vars"
                    (interpret "tests/test5")   1)
    (test-equal?    "Using static scoping instead of dynamic"
                    (interpret "tests/test6")   115)
    (test-true      "Boolean parameters and return values"
                    (interpret "tests/test7"))
    (test-equal?    "Multiple function calls in an expression"
                    (interpret "tests/test8")   20)
    (test-equal?    "Function call in parameter of a function"
                    (interpret "tests/test9")   24)
    (test-equal?    "Function call that ignores return value"
                    (interpret "tests/tests10") 2)
    (test-equal?    "Function without a return statement"
                    (interpret "tests/test11")  35)
    (test-exn       "Mismatched parameters"
                    exn:fail? (interpret "tests/test12"))
    (test-equal?    "Nested functions"
                    (interpret "tests/test13")  90)
    (test-equal?    "Functions inside functions accessing variables outside"
                    (interpret "tests/test14")  69)
    (test-equal?    "Nested functions with vars of the same name"
                    (interpret "tests/test15")  87)
    (test-equal?    "Triple-nested functions"
                    (interpret "tests/test16")  64)
    (test-exn       "Nested function accessing variables out of its scope"
                    exn:fail? (interpret "tests/test17"))
    (test-equal?    "Try/catch/finally without throwing an exception"
                    (interpret "tests/test18")  125)
    (test-equal?    "Throwing exception inside a function"
                    (interpret "tests/test19")  100)
    (test-equal?    "Throwing exception from a function"
                    (interpret "tests/test20")  2000400)
    ))

(run-tests part3-tests)
