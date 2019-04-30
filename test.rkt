#lang racket

(require rackunit "interpreter.rkt")
(require rackunit/text-ui)

;; Part 4 tests
(define part4-tests
  (test-suite
    "Tests for part 4"
    (test-equal?    "Part 4 test 1"
                    (interpret "tests/test4-1" "A")         15)
    (test-equal?    "Part 4 test 2"
                    (interpret "tests/test4-2" "A")         12)
    (test-equal?    "Part 4 test 3"
                    (interpret "tests/test4-3" "A")         125)
    (test-equal?    "Part 4 test 4"
                    (interpret "tests/test4-4" "A")         36)
    (test-equal?    "Part 4 test 5"
                    (interpret "tests/test4-5" "A")         54)
    (test-equal?    "Part 4 test 6"
                    (interpret "tests/test4-6" "A")         110)
    (test-equal?    "Part 4 test 7"
                    (interpret "tests/test4-7" "C")         26)
    (test-equal?    "Part 4 test 8"
                    (interpret "tests/test4-8" "Square")    117)
    (test-equal?    "Part 4 test 9"
                    (interpret "tests/test4-9" "Square")    32)
    (test-equal?    "Part 4 test 10"
                    (interpret "tests/test4-10" "List")     15)
    (test-equal?    "Part 4 test 11"
                    (interpret "tests/test4-11" "List")     123456)
    (test-equal?    "Part 4 test 12"
                    (interpret "tests/test4-12" "List")     5285)
    (test-equal?    "Part 4 test 13"
                    (interpret "tests/test4-13" "C")        -716)
    ))

;; Run suite of tests for part 3
(define part3-tests
  (test-suite
    "Tests for part 3"

    (test-equal?    "A main with code inside"
                    (interpret "tests/test3-1")   10)
    (test-equal?    "A function that uses global variables"
                    (interpret "tests/test3-2")   14)
    (test-equal?    "A function that changes global variables"
                    (interpret "tests/test3-3")   45)
    (test-equal?    "A recursive function"
                    (interpret "tests/test3-4")   55)
    (test-equal?    "Functions with multiple params that hide global vars"
                    (interpret "tests/test3-5")   1)
    (test-equal?    "Using static scoping instead of dynamic"
                    (interpret "tests/test3-6")   115)
    (test-equal?    "Boolean parameters and return values"
                    (interpret "tests/test3-7")   'true)
    (test-equal?    "Multiple function calls in an expression"
                    (interpret "tests/test3-8")   20)
    (test-equal?    "Function call in parameter of a function"
                    (interpret "tests/test3-9")   24)
    (test-equal?    "Function call that ignores return value"
                    (interpret "tests/test3-10")   2)
    (test-equal?    "Function without a return statement"
                    (interpret "tests/test3-11")  35)
    (test-exn       "Mismatched parameters"
                    exn:fail? (interpret "tests/test3-12"))
    (test-equal?    "Nested functions"
                    (interpret "tests/test3-13")  90)
    (test-equal?    "Functions inside functions accessing variables outside"
                    (interpret "tests/test3-14")  69)
    (test-equal?    "Nested functions with vars of the same name"
                    (interpret "tests/test3-15")  87)
    (test-equal?    "Triple-nested functions"
                    (interpret "tests/test3-16")  64)
    (test-exn       "Nested function accessing variables out of its scope"
                    exn:fail? (interpret "tests/test3-17"))
    (test-equal?    "Try/catch/finally without throwing an exception"
                    (interpret "tests/test3-18")  125)
    (test-equal?    "Throwing exception inside a function"
                    (interpret "tests/test3-19")  100)
    (test-equal?    "Throwing exception from a function"
                    (interpret "tests/test3-20")  2000400)
    ))

(define part2-tests
  (test-suite
    "Tests for part 2"

    (test-equal?    "Test 1"
                    (interpret "tests/test2-1")   20)
    (test-equal?    "Test 2"
                    (interpret "tests/test2-2")   164)
    (test-equal?    "Test 3"
                    (interpret "tests/test2-3")   32)
    (test-equal?    "Test 4"
                    (interpret "tests/test2-4")   2)
    (test-exn       "Test 5"
                    exn:fail? (interpret "tests/test2-5"))
    (test-equal?    "Test 6"
                    (interpret "tests/test2-6")   25)
    (test-equal?    "Test 7"
                    (interpret "tests/test2-7")   21)
    (test-equal?    "Test 8"
                    (interpret "tests/test2-8")   6)
    (test-equal?    "Test 9"
                    (interpret "tests/test2-9")   -1)
    (test-equal?    "Test 10"
                    (interpret "tests/test2-10")   789)
    (test-exn       "Test 11"
                    exn:fail? (interpret "tests/test2-11"))
    (test-exn       "Test 12"
                    exn:fail? (interpret "tests/test2-12"))
    (test-exn       "Test 13"
                    exn:fail? (interpret "tests/test2-13"))
    (test-equal?    "Test 14"
                    (interpret "tests/test2-14")  12)
    (test-equal?    "Test 15"
                    (interpret "tests/test2-15")  125)
    (test-equal?    "Test 16"
                    (interpret "tests/test2-16")  110)
    (test-equal?    "Test 17"
                    (interpret "tests/test2-17")  2000400)
    (test-equal?    "Test 18"
                    (interpret "tests/test2-18")  101)
    (test-exn       "Test 19"
                    exn:fail? (interpret "tests/test2-19"))
    ))

(run-tests part4-tests)
