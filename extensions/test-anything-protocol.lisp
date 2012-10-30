;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp -*-
#|

  Test Anything Protocol (TAP) support for LISP-UNIT

  Copyright (c) 2009-2012, Ryan Davis

  Permission is hereby granted, free of charge, to any person obtaining 
  a copy of this software and associated documentation files (the "Software"), 
  to deal in the Software without restriction, including without limitation 
  the rights to use, copy, modify, merge, publish, distribute, sublicense, 
  and/or sell copies of the Software, and to permit persons to whom the 
  Software is furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included 
  in all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS 
  OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL 
  THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR 
  OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, 
  ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR 
  OTHER DEALINGS IN THE SOFTWARE.

  References
  [TAP] http://testanything.org/wiki/index.php/Main_Page
  
|#

(in-package :lisp-unit)

;;; Symbols exported from the TAP extension

(export '(with-tap-output))


(defmacro with-tap-output ((path) &body body)
  "write test results in TAP format to the provided path. If no tests are run
in the body, no test file will be written."
  `(call-with-tap-output ,path #'(lambda () ,@body)))

(defun call-with-tap-output (path bodyfn)
  (check-type path (or string pathname))
  (ensure-directories-exist path)
  (handler-bind
      ((test-failure (lambda (f) (break "Write out failure ~a" f)))
       (test-error (lambda (e) (break "Write out error ~a" e)))
       (test-complete (lambda (c) (break "Write out completion ~a" c))))
    (let ((*signal-test-events-p* T))
      (funcall bodyfn))))


