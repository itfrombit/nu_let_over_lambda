;; @file       test_let_over_lambda.nu
;; @discussion Exercises the functions and macros in let_over_lambda.nu
;;
;; @copyright  Copyright (c) 2009 Jeff Buck
;;
;;   Licensed under the Apache License, Version 2.0 (the "License");
;;   you may not use this file except in compliance with the License.
;;   You may obtain a copy of the License at
;;
;;       http://www.apache.org/licenses/LICENSE-2.0
;;
;;   Unless required by applicable law or agreed to in writing, software
;;   distributed under the License is distributed on an "AS IS" BASIS,
;;   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;   See the License for the specific language governing permissions and
;;   limitations under the License.


(load "with_test")
(load "let_over_lambda")

;(set show-verbose-output t)

(with-test-class TestLetOverLambdaGen
     (with-test-case testBlockScanner
          (set scanner (block-scanner "nuke"))
          (assert_equal nil (scanner "When you write"))
          (assert_equal nil (scanner "nu programs"))
          (assert_equal nil (scanner "you should use nu"))
          (assert_equal t (scanner "ke as your build tool.")))
     
     (with-test-case testBlockScanner2
          (set scanner (block-scanner "nuke"))
          (assert_equal nil (scanner "When you write"))
          (assert_equal nil (scanner "nu programs"))
          (assert_equal nil (scanner "you should use nu"))
          (assert_equal t (scanner "ke as your build tool.")))
     
     (with-test-case testMakeCounter
          (set c1 (make-counter))
          (assert_equal 'up (get-direction))
          (assert_equal 1 (c1))
          (assert_equal 2 (c1))
          (assert_equal 3 (c1))
          (assert_equal 4 (c1))
          (assert_equal 5 (c1))
          (toggle-counter-direction)
          (assert_equal 'down (get-direction))
          (assert_equal 4 (c1))
          (assert_equal 3 (c1))
          (set c2 (make-counter))
          (assert_equal -1 (c2))
          (assert_equal -2 (c2)))
     
     
     ;; Chapter 3 - Control Structures
     
     (with-test-case testUnitOfTime
          (assert_equal 1 (unit-of-time 1 s))
          (assert_equal 60 (unit-of-time 1 m))
          (assert_equal 3600 (unit-of-time 1 h))
          (assert_equal 86400 (unit-of-time 1 d))
          (assert_equal (/ 1 1000) (unit-of-time 1 ms))
          (assert_equal (/ 1 1000000) (unit-of-time 1 us))
          (assert_equal 2 (unit-of-time 2 s))
          (assert_equal 300 (unit-of-time 5 m))
          (assert_equal (* 60 60 24 4) (unit-of-time 4 d))
          (assert_equal (* 60 60 24 14) (unit-of-time 2 w)))
     
     (with-test-case testNlet
          (function nfact (n)
               (nlet fact ((n n))
                     (if (eq n 0)
                         (then 1)
                         (else (* n (fact (- n 1)))))))
          (assert_equal 120 (nfact 5)))
     
     (with-test-case testNif
          (assert_equal '("+" "-" "0")
               (map (do (x) (nif x "+" "0" "-"))
                    '(4 -1 0)))
          
          (assert_equal '("plus" "minus" "zero")
               (map (do (x) (nif x "plus" "zero" "minus"))
                    '((+ 0 2) (- 0 2) (- 2 2)))))
     
     
     (with-test-case testPredicateSplitter
          ;; Put all colors that have 5 letters first
          (assert_equal '("black" "brown" "green" "white" "blue" "orange" "purple" "red" "yellow")
               (sort '("red" "brown" "black" "orange" "blue" "white" "yellow" "purple" "green")
                     (predicate-splitter < (do (n) (eq 5 (n length))))))
          
          ;; Put all colors that end with "e" first
          (assert_equal '("blue" "orange" "purple" "white" "black" "brown" "green" "red" "yellow")
               (sort '("red" "brown" "black" "orange" "blue" "white" "yellow" "purple" "green")
                     (predicate-splitter < (do (n) (eq "e" (subseq n (- (n length) 1) (n length)))))))
          
          ;; Put all even numbers first and sort descending
          (assert_equal '(8 6 4 2 9 7 5 3 1)
               (sort '(5 1 2 4 3 8 9 6 7)
                     (predicate-splitter > evenp))))
     
     
     (with-test-case testTreeLeaves
          (assert_equal '(1 even-number (3 even-number (5 even-number)))
               (tree-leaves '(1 2 (3 4 (5 6)))
                    (evenp it)
                    'even-number))
          
          ;; Multiply even numbers by 10 using captured "it"
          (assert_equal '(1 20 (3 40 (5 60)))
               (tree-leaves '(1 2 (3 4 (5 6)))
                    (evenp it)
                    (* it 10))))
     
     
     (with-test-case testCxr
          (assert_equal 'f (cxr (1 a 5 d) '(a b c d e f g h)))
          (assert_equal 'g (cxr (2 a 3 d) '((a b) (c d) (e f) (g h))))
          (assert_equal 'b (cxr (1 a 1 d 1 a) '((a b) (c d) (e f) (g h)))))
     
     
     (with-test-case testDlambda
          (set counter
               (let ((count 0))
                    (dlambda
                            (reset: () (set count 0))
                            (inc: (n) (incf count n))
                            (dec: (n) (decf count n))
                            (bound: (lo hi)
                             (set count (min hi (max lo count))))
                            (double: () (set count (* 2 count)))
                            (square: () (set count (* count count)))
                            (else (*args) (set count 666)))))
          
          (assert_equal 1 (counter inc: 1))
          (assert_equal 2 (counter inc: 1))
          (assert_equal 12 (counter inc: 10))
          (assert_equal 11 (counter dec: 1))
          (assert_equal 666 (counter))	;; should trigger else clause
          (assert_equal 667 (counter inc: 1))
          (assert_equal 0 (counter reset:))
          (assert_equal -1 (counter dec: 1))
          (assert_equal -2 (counter dec: 1))
          (assert_equal -4 (counter double:))
          (assert_equal 16 (counter square:))
          (assert_equal 116 (counter inc: 100))
          (assert_equal 10 (counter bound: -10 10))
          (assert_equal -90 (counter dec: 100))
          (assert_equal -10 (counter bound: -10 10)))
     
     
     (with-test-case testDlambda2
          ;; Calculate multiple results per invocation.
          (set calc-test
               (alet ((s 0) (m 1) (e 2))
                     (this reset:)
                     (dlambda
                             (reset: ()
                              (set s 0)
                              (set m 1)
                              (set e 2)
                              (list s m e))
                             (else (n)
                                   (set s (+ s n))
                                   (set m (* m n))
                                   (set e (pow e n))
                                   (list s m e)))))
          
          (assert_equal '(0 1 2) (calc-test reset:))
          (assert_equal '(2 2 4) (calc-test 2))
          (assert_equal '(4 4 16) (calc-test 2))
          (assert_equal '(6 8 256) (calc-test 2))
          (assert_equal '(8 16 65536) (calc-test 2))
          
          ;; Try it with a number < 1
          (assert_equal '(0 1 2) (calc-test reset:))
          (assert_equal `(0.5 0.5 ,(pow 2 0.5)) (calc-test 0.5))
          (assert_equal `(,(+ 0.5 0.5) ,(* 0.5 0.5)
                           ,(pow (pow 2 0.5) 0.5)) (calc-test 0.5))
          (assert_equal `(,(+ 0.5 0.5 0.5) ,(* 0.5 0.5 0.5)
                           ,(pow (pow (pow 2 0.5) 0.5) 0.5)) (calc-test 0.5)))
     
     
     (with-test-case testAdo
          ;; List numbers from n to 1
          ;; Use "self" to refer to the anonymous function
          (assert_equal '(10 9 8 7 6 5 4 3 2 1)
               ((ado (n)
                     (if (> n 0)
                         (then (cons n (self (- n 1))))))
                10)))
     
     
     (with-test-case testAif
          ;; Captures result of test expression as "it"
          (assert_equal 25
               (aif (* 5 5)
                    (then it)
                    (else "fail")))
          
          (assert_equal "fail"
               (aif (evenp (* 5 5))
                    (then it)
                    (else "fail"))))
     
     
     (with-test-case testAlet
          ;; Simple counter with an inverter.
          ;; The function to execute is stored in implicit "this".
          ;; Swaps anonymous function "pointer" on invert:
          (set alet-test
               (alet ((acc 0))
                     (ado (n)
                          (if (eq n invert:)
                              (then
                                   (set this
                                        (do (n)
                                            (if (eq n invert:)
                                                (then (set this self))
                                                (else (decf acc n))))))
                              (else (incf acc n))))))
          
          (assert_equal 10 (alet-test 10))
          (assert_equal 11 (alet-test 1))
          (assert_equal 15 (alet-test 4))
          (alet-test invert:)
          (assert_equal 10 (alet-test 5))
          (assert_equal 9 (alet-test 1))
          (alet-test invert:)
          (assert_equal 10 (alet-test 1))
          (assert_equal 11 (alet-test 1))
          
          ;; This time, use local functions.
          ;; This make it more readable and
          ;; better for larger number of cases.
          (set alet-test2
               (alet ((acc 0))
                     (function going-up (n)
                          (if (eq n invert:)
                              (then (set this 'going-down))
                              (else (incf acc n))))
                     (function going-down (n)
                          (if (eq n invert:)
                              (then (set this 'going-up))
                              (else (decf acc n))))
                     'going-up))
          
          (assert_equal 10 (alet-test2 10))
          (assert_equal 11 (alet-test2 1))
          (assert_equal 15 (alet-test2 4))
          (alet-test2 invert:)
          (assert_equal 10 (alet-test2 5))
          (assert_equal 9 (alet-test2 1))
          (alet-test2 invert:)
          (assert_equal 10 (alet-test2 1))
          (assert_equal 11 (alet-test2 1)))
     
     
     (with-test-case testAletFsm
          ;; Further simplify previous example above
          ;; by using a finite state machine
          ;; to generate the functions in the
          ;; macro body.
          (set alet-fsm-test
               (alet ((acc 0))
                     (alet-fsm
                              (going-up (n)
                                   (if (eq n invert:)
                                       (then (state 'going-down))
                                       (else (incf acc n))))
                              (going-down (n)
                                   (if (eq n invert:)
                                       (then (state 'going-up))
                                       (else (decf acc n)))))))
          
          (assert_equal 10 (alet-fsm-test 10))
          (assert_equal 11 (alet-fsm-test 1))
          (assert_equal 15 (alet-fsm-test 4))
          (alet-fsm-test invert:)
          (assert_equal 10 (alet-fsm-test 5))
          (assert_equal 9 (alet-fsm-test 1))
          (alet-fsm-test invert:)
          (assert_equal 10 (alet-fsm-test 1))
          (assert_equal 11 (alet-fsm-test 1)))
     
     
     (with-test-case testIchainBefore
          (set icb
               (alet ((acc 0))
                     ;; These should be called in reverse order
                     (ichain-before (puts "icb: ichain-before A"))
                     (ichain-before (puts "icb: ichain-before B"))
                     (ichain-before (puts "icb: ichain-before C"))
                     (ichain-before (puts "icb: Changing from #{acc}"))
                     (do (n)
                         (incf acc n))))
          
          (assert_equal 2 (icb 2))
          (assert_equal 4 (icb 2))
          
          (set icb2
               (alet ((acc 0))
                     (do (n)
                         (ichain-before (puts "icb2: ichain-before"))
                         (incf acc n))))
          
          (set i 1)
          (while (<= i 4)
                 (puts "invocation #{i}")
                 (icb2 i)
                 (incf i))
          (assert_equal (/ (* i (+ i 1)) 2) (icb2 i)))
     
     
     (with-test-case testIchainAfter
          (set icba
               (alet ((acc 0))
                     (ichain-before (puts "icba: Changing from #{acc}"))
                     (ichain-after  (puts "icba: Changed to #{acc}"))
                     (do (n)
                         (incf acc n))))
          (assert_equal 7 (icba 7))
          )
     
     
     (with-test-case testIchainIntercept
          (set ici
               (alet ((acc 0))
                     (ichain-intercept
                                      (if (< acc 0)
                                          (then
                                               (puts "ici: acc went negative: #{acc}")
                                               (set acc 0)
                                               (intercept acc))))
                     (do (n)
                         (incf acc n))))
          (assert_equal 1 (ici 1))
          (assert_equal 0 (ici -8))
          (assert_equal 3 (ici 3))
          (assert_equal 0 (ici -8)))
     
     
     (with-test-case testAletHotpatch
          (set hotpatch-test
               (alet-hotpatch ((acc 0))
                    (do (n)
                        (incf acc n))))
          
          (assert_equal 3 (hotpatch-test 3))
          (assert_equal 7 (hotpatch-test 4))
          
          (hotpatch-test
                        hotpatch:
                        (let ((acc 0))
                             (do (n)
                                 (incf acc (* 2 n)))))
          
          (assert_equal 4 (hotpatch-test 2))
          (assert_equal 14 (hotpatch-test 5)))
     
     
     (with-test-case testLetHotpatch
          (set hotpatch-test
               (let-hotpatch ((acc 0))
                    (do (n)
                        (incf acc n))))
          
          (assert_equal 3 (hotpatch-test 3))
          (assert_equal 7 (hotpatch-test 4))
          
          (hotpatch-test
                        hotpatch:
                        (let ((acc 0))
                             (do (n)
                                 (incf acc (* 2 n)))))
          
          (assert_equal 4 (hotpatch-test 2))
          (assert_equal 14 (hotpatch-test 5))
          
          ;; make sure 'this' is not captured
          ;; in the 'let' version
          (set hotpatch-test-2
               (let-hotpatch ((acc 0))
                    (do (n)
                        (puts this))))
          
          (assert_throws "NuUndefinedSymbol" (hotpatch-test-2 1)))
     
     
     (with-test-case testLetBindingTransform
          (assert_equal '((a nil) (b nil) (c 5) (d nil))
               (let-binding-transform '(a (b) (c 5) (d nil)))))
     
     (with-test-case testPandoricLetGet
          (set pantest
               (pandoriclet ((acc 0))
                    (do (n) (incf acc n))))
          
          (assert_equal 3 (pantest 3))
          (assert_equal 8 (pantest 5))
          (assert_equal 8 (pantest pandoric-get: 'acc))
          (assert_equal 100 (pantest pandoric-set: 'acc 100))
          (assert_equal 103 (pantest 3))
          )
     
     )





