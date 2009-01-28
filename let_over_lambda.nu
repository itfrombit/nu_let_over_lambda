;; @file       let_over_lambda.nu
;; @discussion A Nu port of examples in Doug Hoyte's book "Let Over Lambda".
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

(load "cl_utils")

;; Missing from math.nu
(set pow (NuBridgedFunction functionWithName:"pow" signature:"ddd"))


;; Chapter 2 - Closures

(function block-scanner (trigger-string)
     (let* ((trig (listify trigger-string))
            (curr trig))
           (do (data-string)
               (let ((data (listify data-string)))
                    (mapcar-1
                             (do (c)
                                 (if curr
                                     (then (set curr
                                                (if (eq (car curr) c)
                                                    (then (cdr curr))
                                                    (else trig))))))
                             data)
                    (not curr)))))



(let ((direction 'up))
     (global toggle-counter-direction
             (do ()
                 (set direction
                      (if (eq direction 'up)
                          (then 'down)
                          (else 'up)))))
     
     (global get-direction
             (do ()
                 direction))
     
     (global make-counter
             (do ()
                 (let ((counter 0))
                      (do ()
                          (if (eq direction 'up)
                              (then (incf counter))
                              (else (decf counter))))))))


;; Chapter 3 - Control structures

(macro-1 unit-of-time (value unit)
     `(* ,value
         ,(case unit
                ('s 1)
                ('m 60)
                ('h 3600)
                ('d 86400)
                ('ms (/ 1 1000))
                ('us (/ 1 1000000))
                )))




;; Implementation of Scheme's named lets
(macro-1 nlet (name letargs *body)
     (puts name letargs)
     `(progn
            (function ,name (,@(mapcar-1 car letargs))
                 ,@*body)
            (,name ,@(mapcar-1 cadr letargs))))


(function nfact (n)
     (nlet fact ((n n))
           (if (eq n 0)
               (then 1)
               (else (* n (fact (- n 1)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(macro-1 nif (expr pos zero neg)
     `(let ((__g ,expr))
           (cond
                ((> __g 0) ,pos)
                ((eq __g 0) ,zero)
                (else ,neg))))



(function predicate-splitter (orderp splitp)
     (do (a b)
         (let ((s (splitp a)))
              (if (eq s (splitp b))
                  (then
                       (if (orderp a b)
                           (then -1)
                           (else 1)))
                  (else
                       (if s
                           (then -1)
                           (else 1)))))))


(function tree-leaves-builder (tree test result)
     (if tree
         (then
              (if (pair? tree)
                  (then
                       (cons
                            (tree-leaves-builder (car tree) test result)
                            (tree-leaves-builder (cdr tree) test result)))
                  (else
                       (if (test tree)
                           (then (result tree))
                           (else tree)))))))

(macro-1 tree-leaves (tree test result)
     `(tree-leaves-builder
                          ,tree
                          (do (it) ,test)
                          (do (it) ,result)))


;; I like Nu's handleUnknownMessage implementation of the arbitrarily
;; long car/cdr combinations much better.
(macro-1 cxr (x tree)
     (if (null? x)
         (then tree)
         (else
              `(,(cond
                      ((eq 'a (cadr x)) 'car)
                      ((eq 'd (cadr x)) 'cdr)
                      (else nil))
                 ,(if (eq 1 (car x))
                      (then
                           `(cxr ,(cddr x) ,tree))
                      (else
                           `(cxr ,(cons (- (car x) 1) (cdr x)) ,tree)))))))



(macro-1 dlambda (*ds)
     `(do (*the_args)
          (if (null? *the_args)
              (then (set *the_args '(nil))))
          (case (car *the_args)
                ,@(mapcar-1
                  (do (d)
                      `(,(if (eq 'else (car d))
                             (then 'else)
                             (else (car d)))
                         (apply (do ,@(cdr d))
                                ,(if (eq 'else (car d))
                                     (then '*the_args)
                                     (else `(cdr *the_args))))))
                  *ds))))


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



;;=====================================================================
;; Chapter 6 Examples on Anaphoric Macros

;; "Anaphoric Macro" is a fancy term for a macro that captures a
;; variable on purpose for convenience.

;; ado: anaphoric do
;; Allows anonymous function to refer to itself as "self"
;; Lisp uses "labels" to keep "self" from polluting the global
;; namespace.  In Nu, we can just wrap a function definition
;; in a "let" call.
(macro-1 ado (params *body)
     `(let ()
           (function self ,params ,@*body)
           self))



;; aif: anaphoric if
;; Works like "if" except the result of testbody is captured
;; in "it".  This prevents having to re-evaluate the testbody
;; later in the macro body.
(macro-1 aif (testbody thenbody *elsebody)
     `(let ((it ,testbody))
           (if it
               (then ,thenbody)
               (else ,*elsebody))))


;; alet: anaphoric let
;; Works like "let" except it binds "this" from the body
;; and binds it to the last expression in the form's body.


;; Note that Nu's "let" cannot accept just a symbol without
;; an initializer. Not a big deal, just different than CL.

(macro-1 alet (letargs *body)
     `(let ((this nil) ,@letargs)
           (set this ,@(last *body))
           ,@(butlast *body)
           (do (*params)
               (apply this *params))))

(puts "------- alet-test -------")
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

(puts (alet-test 10))
(puts (alet-test 1))
(puts (alet-test 4))
(alet-test invert:)
(puts (alet-test 5))
(puts (alet-test 1))
(alet-test invert:)
(puts (alet-test 1))
(puts (alet-test 1))


(puts "------- alet-test2 -------")
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

(puts (alet-test2 10))
(puts (alet-test2 1))
(puts (alet-test2 4))
(alet-test2 invert:)
(puts (alet-test2 5))
(puts (alet-test2 1))
(alet-test2 invert:)
(puts (alet-test2 1))
(puts (alet-test2 1))



(macro-1 alet-fsm (*states)
     `(progn
            (function state (s)
                 (set this s))
            ,@(mapcar-1 (do (s)
                        `(function ,@s))
              *states)
            ,(caar *states)))


(puts "------- alet-fsm-test -------")
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

(puts (alet-fsm-test 10))
(puts (alet-fsm-test 1))
(puts (alet-fsm-test 1))
(puts (alet-fsm-test 4))
(alet-fsm-test invert:)
(puts (alet-fsm-test 1))
(puts (alet-fsm-test 1))


(puts "------- calc-test -------")
(set calc-test
     (alet ((s 0) (m 1) (e 2))
           (this reset:)
           (dlambda
                   (reset: ()
                    (set s 0)
                    (set m 1)
                    (set e 2))
                   (else (n)
                         (set s (+ s n))
                         (set m (* m n))
                         (set e (pow e n))
                         (list s m e)))))


(puts (calc-test reset:))
(puts (calc-test 2))
(puts (calc-test 2))
(puts (calc-test 2))
(puts (calc-test 2))
(puts (calc-test 2))


(puts "------- calc-test part 2 -------")
(puts (calc-test reset:))
(puts (calc-test 0.5))
(puts (calc-test 0.5))
(puts (calc-test 0.5))
(puts (calc-test 0.5))


;;=====================================================================
;; Tests
;; TODO - put these in a separate file...


(puts "------- weave-test -------")
(puts "mc: #{(weave '(7 8 9) '(1 2 3))}")
(puts "mc: #{(weave '(a b c) '(x y z))}")
(puts "mc: #{(weave nil)}")
(puts "mc: #{(weave)}")

(puts "------- cars-test -------")
(puts (cars '((a b c) (1 2 3) (x y z))))
(puts "------- cdrs-test -------")
(puts (cdrs '((a b c) (1 2 3) (x y z))))

(puts (mapcar-1 (do (x) (* x 10)) '(1 2 3)))
(puts (mapcar-1 car '((a b c) (x y z))))


;(puts (mkstr "a" "b" "c"))
;(puts (symb "a" "b" "c"))
;(puts (nthcdr 6 '(a b c d e f g)))
;
;(puts (subseq "abcdefg" 5 7))
;(puts (subseq '(a b c d e f g) 2 4))
;


;(puts (butlast '(a b c d e)))
;(puts (butlast '(a b c d e) 3))
;(puts (butlast '(a b c d e) 4))
;(puts (butlast '(a b c d e) 5))
;(puts (butlast '(a b c d e) 6))


;(puts (group '(a b c d e f g h) 2))
;
;(puts (flatten '(((a b (c d (e)) (f g))))))
;
;(puts (fact 4))
;(puts (choose 6 4))

;(puts (unit-of-time 2 d))

;(puts (macrox
;           (nlet fact ((n n))
;                 (if (eq n 0)
;                     (then 1)
;                     (else (* n (fact (- n 1))))))
;           ))

;(puts (nfact 5))

;(puts
;     (map (do (x) (nif x "+" "0" "-"))
;             '(4 -1 0)))


;(puts (select-if (do (x) (evenp x)) '(0 1 2 3)))
;(puts (select-if (do (x) (oddp x)) '(0 1 2 3)))


;(set c1 (make-counter))
;(puts "direction: #{(get-direction)}")
;(c1)
;(c1)
;(c1)
;(c1)
;(puts "c1: #{(c1)}")
;(toggle-counter-direction)
;(puts "direction: #{(get-direction)}")
;(c1)
;(c1)
;(puts "c1: #{(c1)}")
;(set c2 (make-counter))
;(c2)
;(c2)
;(puts "c2: #{(c2)}")
;
;
;(puts
;     (let* ((a 5)
;            (b (+ a 1)))
;           (set c (+ a b))
;           (* c c))
;     )
;
;(puts
;     (let* ((a 5) (b 3))
;           (+ a b))
;     )
;
;(puts
;     (let* ()
;           (+ 20 10)
;           (- 2 3))
;     )


;;; Put all colors that have 5 letters first
;(puts (sort '("red" "brown" "black" "orange" "blue" "white" "yellow" "purple" "green")
;            (predicate-splitter < (do (n) (eq 5 (n length))))))
;
;;; Put all colors that end with "e" first
;(puts (sort '("red" "brown" "black" "orange" "blue" "white" "yellow" "purple" "green")
;            (predicate-splitter < (do (n) (eq "e" (subseq n (- (n length) 1) (n length)))))))
;
;;; Put all even numbers first and sort descending
;(puts (sort '(5 1 2 4 3 8 9 6 7)
;            (predicate-splitter > evenp) ))
;
;
;(puts (tree-leaves '(1 2 (3 4 (5 6)))
;           (evenp it)
;           'even-number))

(puts "------- counter-test -------")
(puts (counter inc: 1))
(puts (counter inc: 1))
(puts (counter inc: 10))
(puts (counter dec: 1))
(puts (counter))	;; should trigger else clause
(puts (counter inc: 1))
(puts (counter reset:))
(puts (counter dec: 1))
(puts (counter dec: 1))
(puts (counter double:))
(puts (counter square:))
(puts (counter inc: 100))
(puts (counter bound: -10 10))
(puts (counter dec: 100))
(puts (counter bound: -10 10))


;(puts (macrox
;	(cxr (1 a 5 d) '(a b c d e f g h))
;))
;(puts (cxr (1 a 5 d) '(a b c d e f g h)))
;(puts (cxr (2 a 3 d) '((a b) (c d) (e f) (g h))))
;(puts (cxr (1 a 1 d 1 a) '((a b) (c d) (e f) (g h))))


;; Try out anaphoric do:
;; List numbers from n to 1
(puts "------- ado-test -------")
(puts ((ado (n)
            (if (> n 0)
                (then (cons n (self (- n 1))))))
       10))


;; Test aif:
(puts "------- aif-test -------")
(aif (* 5 5)
     (then (puts it))
     (else (puts "fail")))

