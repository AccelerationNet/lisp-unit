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

(defvar *tap-out-stream* nil "where to write TAP output to")
(defvar *tap-test-count* nil "how many tests we;ve seen so far")

(defmacro with-yaml-block (() &body body)
  `(progn
    (format *tap-out-stream* "  ---~%")
    ,@body
    (format *tap-out-stream* "  ...~%")))

(defgeneric tap-output (event)
  (:documentation "writes TAP output"))

(defmethod tap-output ((f test-failure))
  "write out the failure info"
  (format *tap-out-stream* "not ok ~d ~a~%" (incf *tap-test-count*) (name (test f)))
  (with-yaml-block ()
    (apply #'tap-output/indented "expected" (expected f))
    (apply #'tap-output/indented "actual" (actual f))
    ;; extras is a function that returns a plist of key/values
    (when (extras f)
      (let ((extras-plist (funcall (extras f))))
        ;; walk the plist
        (loop for (key value) on extras-plist by #'cddr
              ;; if the key == value, then it's likely just a description of
              ;; the assertion.
              if (equalp key value) do (tap-output/indented "message" key)
              ;; otherwise it's likely a binding
              else do (tap-output/indented key value))))))

(defmethod tap-output ((f test-error))
  "print the error type and message"
  (format *tap-out-stream* "not ok ~d ~a~%" (incf *tap-test-count*) (name (test f)))
  (with-yaml-block ()
    (tap-output/indented "error"
                         (type-of (error-condition f))
                         ;; convert to string first; default output will looks
                         ;; like `#<type msg>`, and `#` is the comment
                         ;; character in TAP.
                         (princ-to-string (error-condition f)))))

(defmethod tap-output ((f test-complete))
  "write out the successful test"
  (when (plusp (passed f))
    (format *tap-out-stream* "ok ~d ~a (~d assertions) ~%"
            (incf *tap-test-count*) (name (test f)) (passed f))))

(defmethod tap-output ((evt (eql :done)))
  (format *tap-out-stream* "1..~d~%" *tap-test-count*))

(defun tap-output/indented (label &rest datum)
  "write an indented block with a label"
  (format *tap-out-stream* "  ~a: |~%" label)
  ;; see http://www.lispworks.com/documentation/HyperSpec/Body/22_ceb.htm
  ;; complicated format string adds 4 space indentation regardless of newlines
  ;; in the datum.
  (format *tap-out-stream* "~<    ~@;~{~S~^~%~}~:>~%" (list datum)))

(defun call-with-tap-output (path bodyfn)
  (check-type path (or string pathname))
  (ensure-directories-exist path)
  (let ((*tap-test-count* 0)
        (*signal-test-events-p* T)
        (*tap-out-stream* T))
    (with-open-file (*tap-out-stream* path :direction :output :if-exists :supersede)
      (handler-bind
          ((test-failure #'tap-output)
           (test-error #'tap-output)
           (test-complete #'tap-output))
        (funcall bodyfn)
        (tap-output :done)))
    (if (zerop *tap-test-count*)
        (delete-file path)
        (truename path))))

(defmacro with-tap-output ((path) &body body)
  "write test results in TAP format to the provided path. If no tests are run
in the body, no test file will be written."
  `(call-with-tap-output ,path #'(lambda () ,@body)))
