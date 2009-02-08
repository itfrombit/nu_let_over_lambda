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
(set sqrt (NuBridgedFunction functionWithName:"sqrt" signature:"dd"))


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
                ('w (* 86400 7))
                ('ms (/ 1 1000))
                ('us (/ 1 1000000))
                )))




;; Implementation of Scheme's named lets
(macro-1 nlet (name letargs *body)
     `(progn
            (function ,name (,@(mapcar-1 car letargs))
                 ,@*body)
            (,name ,@(mapcar-1 cadr letargs))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; nif: Numeric if
;; Works like "if" except there are three potential
;; code paths depending on whether the results of expr
;; is positive, zero, or negative.
(macro-1 nif (expr pos zero neg)
     `(let ((__g ,expr))
           (cond
                ((> __g 0) ,pos)
                ((eq __g 0) ,zero)
                (else ,neg))))


;; predicate-splitter first partitions the list by
;; the test in splitp, then sorts each partition by
;; orderp.  The result is a single list sorted by
;; splitp, then orderp.
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


;; Helper function for tree-leaves macro below
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

;; tree-leaves: perform a function on all leaves
;; of a tree.  The test and result functions can
;; make use of the captured "it" variable.
(macro-1 tree-leaves (tree test result)
     `(tree-leaves-builder
                          ,tree
                          (do (it) ,test)
                          (do (it) ,result)))


;; I like Nu's handleUnknownMessage implementation of the arbitrarily
;; long car/cdr combinations much better.
;; Plus, cxr is very brittle if you pass it bad args.
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
;;
;; Note that Nu's "let" cannot accept just a symbol without
;; an initializer. Not a big deal, just different than CL.
(macro-1 alet (letargs *body)
     `(let ((this nil) ,@letargs)
           (set this ,@(last *body))
           ,@(butlast *body)
           (do (*params)
               (apply this *params))))


;; alet-fsm: alet with a finite-state-machine syntax.
;; The book used macro-let for this.  Since Nu doesn't
;; have macro-let, I changed it to a progn and used
;; local functions as the "labels" to represent each
;; state.
;;
;; Each "state" can accept a unique parameter list.
(macro-1 alet-fsm (*states)
     `(progn
            (function state (s)
                 (set this s))
            ,@(mapcar-1 (do (s)
                        `(function ,@s))
              *states)
            ,(caar *states)))


;; Section 6.4 - Indirect chaining examples
;; These work by injecting a closure into the
;; 'this' variable, which is used by alet.

;; ichain-before: execute a body of code before
;; invoking the main closure.

(macro-1 ichain-before (*body)
     `(let ((__indir-env this))
           (set this
                (do (*temp-args)
                    ,@*body
                    (apply __indir-env *temp-args)))))


;; ichain-after: execute a body of code after
;; invoking the main closure.

(macro-1 ichain-after (*body)
     `(let ((__indir-env this))
           (set this
                (do (*temp-args)
                    (progn
                          (set result (apply __indir-env *temp-args))
                          ,@*body
                          result)))))


;; ichain-intercept: execute a body of code
;; when an intercept condition becomes true.
(macro-1 ichain-intercept (*body)
     `(let ((__indir-env this))
           (set this
                (do (*temp-args)
                    (macro-1 intercept (v)
                         (list 'return v))
                    (progn
                          (set result (apply __indir-env *temp-args))
                          ,@*body
                          result)))))


;; Section 6.5: Hotpatching Closures

;; alet-hotpatch: Allow hotpatching of
;; closures.  If you pass 'hotpatch:'
;; as the first argument, you can replace
;; the captured closure with a new one.

(macro-1 alet-hotpatch (letargs *body)
     `(let ((this nil) ,@letargs)
           (set this ,@(last *body))
           ,@(butlast *body)
           (dlambda
                   (hotpatch: (closure)
                    (set this closure))
                   (else (*restargs)
                         (apply this *restargs)))))


;; let-hotpatch: Same as above, but doesn't
;; capture the 'this' anaphor.  If you don't
;; need 'this', this version is safer from
;; unwanted variable capture.
(macro-1 let-hotpatch (letargs *body)
     `(let ((__this nil) ,@letargs)
           (set __this ,@(last *body))
           ,@(butlast *body)
           (dlambda
                   (hotpatch: (closure)
                    (set __this closure))
                   (else (*restargs)
                         (apply __this *restargs)))))


;; Section 6.7: Pandoric Macros

;; Utility function to create a Nu-compliant
;; let bindings list from a list of atoms,
;; single list symbols, or full let binding
;; expression.
(function let-binding-transform (bs)
     (if bs
         (then
              (cons
                   (cond
                        ((symbol? (car bs))
                         ;; a standalone symbol
                         (list (car bs) 'nil))
                        ((pair? (car bs))
                         (if (eq 1 ((car bs) length))
                             (then
                                  ;; a symbol in a list
                                  ;; with no initial value
                                  (list (caar bs) 'nil))
                             (else
                                  ;; use as-is
                                  (car bs))))
                        (else
                             (throw "LetBindingTransformException")))
                   (let-binding-transform (cdr bs))))))


;; These are two helper functions for the pandoriclet
;; macro defined below.
(function pandoriclet-get (letargs)
     `(cond
           ,@(mapcar-1 (do (a) `((eq sym ,(car a))
                                ,(car a))) letargs)
           (else
                (throw "PandoricLetGetException"))))

(function pandoriclet-set (letargs)
     `(cond
           ,@(mapcar-1 (do (a) `((eq sym ,(car a)) (set ,(car a) val))) letargs)
           (else
                (throw "PandoricLetSetException"))))


;; pandoriclet: allow the closed-over lexical variables
;; to be accessed externally.
(macro-1 pandoriclet (letargs *body)
     (let ((letargs
                   (cons '(this nil)
                         (let-binding-transform letargs))))
          `(let (,@letargs)
                (set this ,@(last *body))
                ,@(butlast *body)
                (dlambda
                        (pandoric-get: (sym)
                         ,(pandoriclet-get letargs))
                        (pandoric-set: (sym val)
                         ,(pandoriclet-set letargs))
                        (else (*restargs)
                              (apply this *restargs))))))


;; get-pandoric: shorthand (not by much) to access a
;; pandoric binding.
(function get-pandoric (box sym)
     (box pandoric-get: sym))

;; Nu doesn't have defsetf functionality (it's complicated to
;; implement), so the set-pandoric is explicit instead of as
;; described in the book.  This affects how you can use the
;; pandoric bindings (no sets with shorthand notation).
(function set-pandoric (box sym val)
     (box pandoric-set: sym val))


;; with-pandoric: provide a shorthand notation to access
;; pandoric bindings in the original closure.
(macro-1 with-pandoric (syms box *body)
     `(progn
            (set __box ,box)
            (let
                (,@(mapcar-1 (do (a) `(,a (get-pandoric __box ',a))) syms) )
                ,@*body)))


(function pandoric-hotpatch (box new)
     (set-pandoric box 'this new))


;; pandoric-recode: override the original pandoric function using
;; original pandoric bindings.
(macro-1 pandoric-recode (vars box new)
     `(progn
            (set __box ,box)
            (with-pandoric (this ,@vars) __box
                 (set-pandoric __box 'this ,new))))


;; plambda: turns pandoric-let inside out.
;; Can export any variables in existing lexical environment,
;; making them available in other lexical scopes.
(macro-1 plambda (largs pargs *body)
     (let ((pargs (mapcar-1 list pargs)))
          `(let ((this nil) (self nil))
                (set this (do ,largs ,@*body))
                (set self
                     (dlambda
                             (pandoric-get: (sym)
                              ,(pandoriclet-get pargs))
                             (pandoric-set: (sym val)
                              ,(pandoriclet-set pargs))
                             (else (*restargs)
                                   (apply this *restargs)))))))


;; defpan: shorthand for defining pandoric functions.
;; defpan uses the 'self anaphor and can chain to other
;; defpan functions.
(macro-1 defpan (name args *body)
     `(function ,name (self)
           ,(if args
                (then
                     `(with-pandoric ,args self
                           ,@*body))
                (else
                     `(progn ,@*body)))))


;; How to pass bindings into an otherwise null
;; eval lexical environment.
(set pandoric-eval-tunnel nil)

(macro-1 pandoric-eval (vars expr)
     `(let ((pandoric-eval-tunnel (plambda () ,vars t)))
           (eval
                (list 'with-pandoric
                      ',vars 'pandoric-eval-tunnel
                      ,expr))))














