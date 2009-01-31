;; @file       with_test.nu
;; @discussion Adds wrapper around NuTestCase for logging output option.
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


;; Set to nil to run as normal unit tests.
;; Set to t to see all code and results of assert_equals.
(set show-verbose-output nil)
;(set show-verbose-output t)

;; Set to t to print headers.
;; Has no effect when show-verbose-output is nil.
(set show-test-headers t)


(macro-1 with-test-class (class-name *body)
     (if (not show-verbose-output)
         (then
              `(class ,class-name is NuTestCase
                    ,@*body))
         (else
              `(progn
                     (if show-test-headers
                         (then
                              (print "-------- ")
                              (print ',class-name)
                              (print " --------\n")))
                     ,@*body))))

(macro-1 with-test-case (test-name *body)
     (if (not show-verbose-output)
         (then
              `(imethod (id) ,test-name is
                    ,@*body))
         (else
              `(progn
                     (if show-test-headers
                         (then
                              (print "  -------- ")
                              (print ',test-name)
                              (print " --------\n")))
                     ,@(mapcar-1
                       (do (statement)
                           (if (eq 'assert_equal (car statement))
                               (then
                                    `(progn
                                           (print ',@(cddr statement))
                                           (print " -> ")
                                           (print ,@(cddr statement))
                                           (print "\n")))
                               (else
                                    statement)))
                       *body)))))
