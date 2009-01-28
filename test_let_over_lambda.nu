(load "let_over_lambda")

;; Set to t to run as normal unit tests.
;; Set to nil to see all asserts as puts.
(set do-unit-testing t)

;; Set to t to print headers.
;; Has no effect when do-unit-testing is t.
(set do-header-printing t)


(macro-1 with-test-class (class-name *body)
     (if do-unit-testing
         (then
              `(class ,class-name is NuTestCase
                    ,@*body))
         (else
              `(progn
                     (if do-header-printing
                         (then
                              (print "-------- ")
                              (print ',class-name)
                              (print " --------\n")))
                     ,@*body))))

(macro-1 with-test-case (test-name *body)
     (if do-unit-testing
         (then
              `(imethod (id) ,test-name is
                    ,@*body))
         (else
              `(progn
                     (if do-header-printing
                         (then
                              (print "  -------- ")
                              (print ',test-name)
                              (print " --------\n")))
                     ,@(mapcar-1
                       (do (statement)
                           (if (eq 'assert_equal (car statement))
                               (then
                                    `(puts ,@(cddr statement)))
                               (else
                                    statement)))
                       *body)))))


;(puts (macrox
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
     )
;           ))

;(class TestLetOverLambda is NuTestCase
;     (imethod (id) testBlockScanner is
;          (set scanner (block-scanner "nuke"))
;          (assert_equal nil (scanner "When you write"))
;          (assert_equal nil (scanner "nu programs"))
;          (assert_equal nil (scanner "you should use nu"))
;          (assert_equal t (scanner "ke as your build tool."))))


