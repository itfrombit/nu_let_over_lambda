;; @file       test_cl_utils.nu
;; @discussion Exercises the functions and macros in cl_utils.nu
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
(load "cl_utils")

;(set show-verbose-output t)

(with-test-class TestClUtils
     (with-test-case testMapcar
          (assert_equal '(10 20 30) (mapcar-1 (do (x) (* x 10)) '(1 2 3)))
          (assert_equal '() (mapcar-1 (do (x) (* x 10)) nil))
          (assert_equal '(() () ()) (mapcar-1 (do (x) nil) '(1 2 3)))
          (assert_equal '(a x) (mapcar-1 car '((a b c) (x y z)))))
     
     (with-test-case testCars
          (assert_equal 'a (caar '((a b) (c d))))
          (assert_equal '(c d) (cadr '((a b) (c d))))
          (assert_equal '((e f)) (cddr '((a b) (c d) (e f))))
          (assert_equal 'c (car (cadr '((a b) (c d)))))
          (assert_equal '(a 1 x) (cars '((a b c) (1 2 3) (x y z))))
          (assert_equal '((b c) (2 3) (y z)) (cdrs '((a b c) (1 2 3) (x y z)))))
     
     (with-test-case testIncf
          (set a 0)
          (assert_equal 1 (incf a))
          (assert_equal 1 a)
          (assert_equal 5 (incf a 4))
          (assert_equal 5 a)
          (assert_equal 3 (incf a -2))
          (assert_equal 3 a))
     
     (with-test-case testDecf
          (set a 0)
          (assert_equal -1 (decf a))
          (assert_equal -1 a)
          (assert_equal -5 (decf a 4))
          (assert_equal -5 a)
          (assert_equal -3 (decf a -2))
          (assert_equal -3 a))
     
     (with-test-case testEvenp
          (assert_equal '(t t t () () ()) (mapcar-1 evenp '(2 0 -4 1 3 7)))
          (assert_equal '(t t t () () ()) (mapcar-1 even? '(2 0 -4 1 3 7)))
          (assert_equal '(() () () t t t) (mapcar-1 oddp '(2 0 -4 1 3 7)))
          (assert_equal '(() () () t t t) (mapcar-1 odd? '(2 0 -4 1 3 7))))
     
     (with-test-case testSelectIf
          (assert_equal '() (select-if (do (x) nil) '(0 1 2 3)))
          (assert_equal '() (select-if (do (x) (evenp x)) '()))
          (assert_equal '(0 2) (select-if (do (x) (evenp x)) '(0 1 2 3)))
          (assert_equal '(1 3) (select-if (do (x) (oddp x)) '(0 1 2 3))))
     
     (with-test-case testNthcdr
          (assert_equal '(a b c d e f g) (nthcdr 0 '(a b c d e f g)))
          (assert_equal '(b c d e f g) (nthcdr 1 '(a b c d e f g)))
          (assert_equal '(d e f g) (nthcdr 3 '(a b c d e f g)))
          (assert_equal '(g) (nthcdr 6 '(a b c d e f g)))
          (assert_equal '() (nthcdr 10 '(a b c d e f g))))
     
     (with-test-case testSubseq
          (assert_equal "abcdefgh" (subseq "abcdefgh" 0 8))
          (assert_equal "cd" (subseq "abcdefgh" 2 4))
          (assert_equal "fg" (subseq "abcdefgh" 5 7))
          (assert_equal "a" (subseq "abcdefgh" 0 1))
          (assert_equal "h" (subseq "abcdefgh" 7 8))
          (assert_equal "" (subseq "abcdefgh" 4 3))
          (assert_equal "" (subseq "abcdefgh" 8 8))
          (assert_equal '(a b c d e f g h) (subseq '(a b c d e f g h) 0 8))
          (assert_equal '(d e f g h) (subseq '(a b c d e f g h) 3 10))
          (assert_equal '(c d) (subseq '(a b c d e f g h) 2 4))
          (assert_equal '(f g) (subseq '(a b c d e f g h) 5 7))
          (assert_equal '(a) (subseq '(a b c d e f g h) 0 1))
          (assert_equal '(h) (subseq '(a b c d e f g h) 7 8))
          (assert_equal '() (subseq '(a b c d e f g h) 4 3))
          (assert_equal '() (subseq '(a b c d e f g h) 8 8)))
     
     (with-test-case testLast
          (assert_equal '(e) (last '(a b c d e)))
          (assert_equal '(c d e) (last '(a b c d e) 3))
          (assert_equal '(a b c d e) (last '(a b c d e) 5))
          (assert_equal '(a b c d e) (last '(a b c d e) 6))
          (assert_equal '() (last '(a b c d e) 0)))
     
     (with-test-case testButlast
          (assert_equal '(a b c d) (butlast '(a b c d e)))
          (assert_equal '(a b) (butlast '(a b c d e) 3))
          (assert_equal '(a) (butlast '(a b c d e) 4))
          (assert_equal '() (butlast '(a b c d e) 5))
          (assert_equal '() (butlast '(a b c d e) 6)))
     
     (with-test-case testLetStar
          (assert_equal 8
               (let* ((a 5) (b 3))
                     (+ a b)))
          
          (assert_equal 4
               (let* ((a 2) (b a))
                     (+ a b)))
          
          (assert_equal 6
               (let* ((a 2) (b (* a a)))
                     (+ a b)))
          
          (assert_equal 121
               (let* ((a 5)
                      (b (+ a 1)))
                     (set c (+ a b))
                     (* c c)))
          
          (assert_equal -1
               (let* ()
                     (- 2 3))))
     
     (with-test-case testMkstr
          (assert_equal "abc" (mkstr "a" "b" "c"))
          (assert_equal 'abc (symb "a" "b" "c")))
     
     (with-test-case testGroup
          (assert_equal '((a) (b) (c) (d) (e) (f) (g) (h)) (group '(a b c d e f g h) 1))
          (assert_equal '((a b) (c d) (e f) (g h)) (group '(a b c d e f g h) 2))
          (assert_equal '((a b c d) (e f g h)) (group '(a b c d e f g h) 4))
          (assert_equal '((a b c d e f g h)) (group '(a b c d e f g h) 8))
          
          (assert_equal '((a b c) (d e f) (g h)) (group '(a b c d e f g h) 3))
          (assert_equal '((a b c d e) (f g h)) (group '(a b c d e f g h) 5))
          (assert_equal '((a b c d e f g h)) (group '(a b c d e f g h) 100)))
     
     (with-test-case testFlatten
          (assert_equal '() (flatten '()))
          (assert_equal '() (flatten '((()))))
          (assert_equal '(a b c d e f g h) (flatten '(a b c d e f g h)))
          (assert_equal '(a b c d e f g h) (flatten '(((a b (c d (e)) (f g (((h)))))))))
          (assert_equal '(a) (flatten '((((a)))))))
     
     (with-test-case testWeave
          (assert_equal '((7 1) (8 2) (9 3)) (weave '(7 8 9) '(1 2 3)))
          (assert_equal '((a x) (b y) (c z)) (weave '(a b c) '(x y z)))
          (assert_equal nil (weave nil))
          (assert_equal nil (weave)))
     
     (with-test-case testListify
          (assert_equal '("a" "b" "c" "d") (listify "abcd"))
          (assert_equal '() (listify ""))
          (assert_equal '("r" "e" "a" "d" "y") (listify (+ "re" "a" "dy"))))
     
     (with-test-case testBinomials
          (assert_equal 1 (fact 0))
          (assert_equal 1 (fact 1))
          (assert_equal 24 (fact 4))
          (assert_equal 15 (choose 6 4))
          (assert_equal 360 (perm 6 4))))


