;; @file       cl_utils.nu
;; @discussion Nu versions of popular Common Lisp functions and macros.
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


;; These functions are part of Common Lisp.
;; The subsequent examples in the book assume they are defined.


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

(set even? evenp)
(set odd? oddp)

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
              (if (>= start end)
                  (then "")
                  (else
                       ;; String - use substring
                       (l substringWithRange:(list start (- end start))))))
         (else
              ;; Assume a list - use cdrs
              (set len (l length))
              (set i start)
              (set result nil)
              (while (and (< i end) (< i len))
                     (set result (append result (list (car (nthcdr i l)))))
                     (set i (+ i 1)))
              result)))


(function last (l *n)
     (let ((len (l length)))
          (if *n
              (then (set count (car *n)))
              (else (set count 1)))
          (if (> count len)
              (then (set count len)))
          (subseq l (- len count) len)))


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


;; Not part of Common Lisp, but popular functions to have around...

;; Glue up a string from various substrings.
(function mkstr (*rest)
     (set s "")
     (*rest each:
            (do (a)
                (set s (+ s a))))
     s)

;; Make a symbol name out of a list of substrings.
(function symb (*rest)
     ((apply mkstr *rest) symbolValue))


;; Group a flat list into lists of length n.
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


;; Flatten a nested list.
(function flatten (x)
     (function flatten-rec (x acc)
          (cond
               ((eq x nil) acc)
               ((atom? x) (cons x acc))
               (else (flatten-rec (car x) (flatten-rec (cdr x) acc)))))
     (flatten-rec x nil))


;; A few math functions
(function fact (x)
     (if (<= x 0)
         (then 1)
         (else (* x (fact (- x 1))))))

(function choose (n r)
     (/ (fact n)
        (fact (- n r))
        (fact r)))

(function perm (n r)
     (/ (fact n)
        (fact (- n r))))


;; The rest of the functions in this file are not part of
;; Common Lisp, but they are "Common Lispy" enough to include
;; here.  They are provided as a convenience functions that a
;; multi-list mapcar could otherwise provide.

;; returns the obvious on a list of lists.
(function cars (lists)
     (mapcar-1 car lists))

(function cdrs (lists)
     (mapcar-1 cdr lists))

;; Nu's map provides similar functionality to weave,
;; but doesn't work for quoted lists like this:
;;   (map list '(a b c) '(x y z))
;; It tries to eval the list elements.
;;  ^ the internal apply is the guilty party in map.

;; weave only works for two lists.
;;  ex: (weave '(a b c) '(x y z)) -> (a x) (b y) (c z)
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


; Nu adds a "list" method to NSArray.
; This function turns a string into a list of characters.
(function listify (s)
     (let ((i 0)
           (len (s length))
           (result '()))
          (while (< i len)
                 (set result (append result (list (subseq s i (+ i 1)))))
                 (set i (+ i 1)))
          result))

