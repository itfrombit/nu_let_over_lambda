;; These functions are part of Common Lisp.
;; The subsequent examples in the book assume they are defined.

(set pow (NuBridgedFunction functionWithName:"pow" signature:"ddd"))

(function mapcar-1 (f l)
     (cond
          ((null? l) nil)
          (else
               (cons (f (car l)) (mapcar-1 f (cdr l))))))

;; Nu's cadr-type built-ins are postfix.
;; Not as suitable for lispy mapping functions.
(function caar (l)
     (car (car l)))

(function cadr (l)
     (car (cdr l)))

(function cddr (x)
     (cdr (cdr x)))


(function cars (lists)
     (mapcar-1 car lists))

(function cdrs (lists)
     (mapcar-1 cdr lists))


;; Not part of Common Lisp, but provided as a convenience
;; function that a multi-list mapcar could otherwise provide.
(function weave (*lists)
     (function weave-rec (lists)
          (cond
               ((null? lists) nil)
               ((null? (car lists)) nil)
               (else
                    (cons
                         (cars lists)
                         (weave-rec (cdrs lists))))))
     (weave-rec *lists))


(macro-1 incf (n *delta)
     (if (not (eq *delta '()))
         (then `(set ,n (+ ,n ,(car *delta))))
         (else `(set ,n (+ ,n 1)))))

(macro-1 decf (n *delta)
     (if (not (eq *delta '()))
         (then `(set ,n (- ,n ,(car *delta))))
         (else `(set ,n (- ,n 1)))))


(function evenp (x)
     ((eq 0 (% x 2))))

(function oddp (x)
     (not (evenp x)))

(function select-if (f l)
     (function select-if-acc (f l acc)
          (if (null? l)
              (then acc)
              (else
                   (if (f (car l))
                       (then (select-if-acc f (cdr l) (append acc (list (car l)))))
                       (else (select-if-acc f (cdr l) acc))))))
     (select-if-acc f l nil))


(function nthcdr (n source)
     (cond ((eq n 0)
            source)
           ((> n (source length)) nil)
           (else (nthcdr (- n 1) (cdr source)))))


(function subseq (l start end)
     (if (eq (l class) ("a" class))
         (then
              ;; String - use substring
              (l substringWithRange:(list start (- end start))))
         (else
              ;; List - use cdrs
              (set i start)
              (set result nil)
              (while (< i end)
                     (set result (append result (list (car (nthcdr i l)))))
                     (set i (+ i 1)))
              result)))


(function last (l)
     (let ((len (l length)))
          (subseq l (- len 1) len)))


(function butlast (l *n)
     (if (not (eq *n '()))
         (then (set count (car *n)))
         (else (set count 1)))
     (let ((len (l length)))
          (if (>= count len)
              (then '())
              (else (subseq l 0 (- len count))))))



(macro-1 let* (bindings *body)
     (if (null? bindings)
         (then
              `(progn
                     ,@*body))
         (else
              (set __nextcall `(let* ,(cdr bindings) ,@*body))
              `(let (,(car bindings))
                    ,__nextcall))))


; Nu actually adds a "list" method to NSArray.
; This function is not necessary...
;(function listify (ar)
;     (cond
;          ((or (eq ar nil) (eq (ar count) 0)) nil)
;          (else
;               (let ((i 0)
;                     (l nil))
;                    (while (< i (ar count))
;                           (set l (append l (list (ar i))))
;                           (set i (+ i 1)))
;                    l))))


(function mkstr (*rest)
     (set s "")
     (*rest each:
            (do (a)
                (set s (+ s a))))
     s)

(function symb (*rest)
     ((apply mkstr *rest) symbolValue))




(function group (source n)
     (function group-rec (source n acc)
          (let ((rest (nthcdr n source)))
               (if (pair? rest)
                   (then
                        (group-rec rest n (cons (subseq source 0 n) acc)))
                   (else
                        (reverse (cons source acc))))))
     (if source
         (then (group-rec source n nil))
         (else nil)))



(function flatten (x)
     (function flatten-rec (x acc)
          (cond
               ((eq x nil) acc)
               ((atom? x) (cons x acc))
               (else (flatten-rec (car x) (flatten-rec (cdr x) acc)))))
     (flatten-rec x nil))

(function fact (x)
     (if (eq x 0)
         (then 1)
         (else (* x (fact (- x 1))))))

(function choose (n r)
     (/ (fact n)
        (fact (- n r))
        (fact r)))


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
(puts "little")


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
(puts "little")

(macro-1 alet-fsm-state (s)
     `(set this ,s))

(macro-1 alet-fsm (*states)
     `(progn
            (function state (s)
                 (set this s))
            ,@(mapcar-1 (do (s)
                        `(function ,@s))
              *states)
            ,(caar *states)))


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



(puts "------- alet-fsm-test -------")
(puts (alet-fsm-test 10))
(puts (alet-fsm-test 1))
(puts (alet-fsm-test 1))
(puts (alet-fsm-test 4))
(alet-fsm-test invert:)
(puts (alet-fsm-test 1))
(puts (alet-fsm-test 1))


(set calc
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


(puts (calc reset:))
(puts (calc 2))
(puts (calc 2))
(puts (calc 2))
(puts (calc 2))
(puts (calc 2))
(puts (calc reset:))
(puts (calc 0.5))
(puts (calc 0.5))
(puts (calc 0.5))
(puts (calc 0.5))


;;=====================================================================
;; Tests

;(puts "mc: #{(weave '(7 8 9) '(1 2 3))}")
;(puts "mc: #{(weave '(a b c) '(x y z))}")
;(puts "mc: #{(weave nil)}")
;(puts "mc: #{(weave)}")

;(puts (cars '(a b c) '(1 2 3) '(x y z)))
;(puts (cdrs '(a b c) '(1 2 3) '(x y z)))

;(puts (mapcar (do (x) (* x 10)) '(1 2 3)))
;(puts (mapcar (do (x y) (* x 10)) '(7 8 9) '(1 2 3)))

;(puts (mapcar car '((a b c) (x y z))))


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
(puts ((ado (n)
            (if (> n 0)
                (then (cons n (self (- n 1))))))
       10))


;; Test aif:
(aif (* 5 5)
     (then (puts it))
     (else (puts "fail")))

