;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp -*-
;;;
;;; Floating tests and assertions for LISP-UNIT
;;;
;;; Copyright (c) 2009 Thomas M. Hermann
;;; 
;;; Permission is hereby granted, free of charge, to any person obtaining 
;;; a copy of this software and associated documentation files (the "Software"), 
;;; to deal in the Software without restriction, including without limitation 
;;; the rights to use, copy, modify, merge, publish, distribute, sublicense, 
;;; and/or sell copies of the Software, and to permit persons to whom the 
;;; Software is furnished to do so, subject to the following conditions:
;;; 
;;; The above copyright notice and this permission notice shall be included 
;;; in all copies or substantial portions of the Software.
;;; 
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS 
;;; OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL 
;;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR 
;;; OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, 
;;; ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR 
;;; OTHER DEALINGS IN THE SOFTWARE.
;;;
;;; References
;;; [NumAlgoC] Gisela Engeln-Mullges and Frank Uhlig "Numerical
;;;            Algorithms with C", Springer, 1996
;;;            ISBN: 3-540-60530-4

(common-lisp:in-package :lisp-unit)

(defparameter *epsilon* nil
  "Set the error epsilon if the defaults are not acceptable.")

(defparameter *significant-figures* 4
  "Default to 4 significant figures.")

;;; (RELATIVE-ERROR x y) => number
;;; [NumAlgoC] : Definition 1.3, pg. 2
;;;              modified with Definition 1.1, pg. 1
;;;
;;; The definition of relative error in this routine is modified from
;;; the Definition 1.3 in [NumAlgoC] for cases when either the exact
;;; or the approximate value equals zero. According to Definition 1.3,
;;; the relative error is identically equal to 1 in those cases. This
;;; function returns the absolue error in those cases. This is more
;;; useful for testing.
(defun relative-error (exact approximate)
  "Return the error delta between the exact and approximate floating
point value."
  (abs (if (or (zerop exact) (zerop approximate))
	   (- exact approximate)
	   (/ (- exact approximate) exact))))

;;; (FLOAT-EQUAL float1 float2 &optional epsilon) => true or false
(defun %float-equal (float1 float2 epsilon)
  "Internal version that does not verify arguments as floats."
  (cond
    ((and (zerop float1) (zerop float2)))
    (epsilon
     (< (relative-error float1 float2) epsilon))
    ((or (typep float1 'short-float) (typep float2 'short-float))
     (< (relative-error float1 float2) (* 2.0 short-float-epsilon)))
    ((or (typep float1 'single-float) (typep float2 'single-float))
     (< (relative-error float1 float2) (* 2.0 single-float-epsilon)))
    ((or (typep float1 'double-float) (typep float2 'double-float))
     (< (relative-error float1 float2) (* 2.0 double-float-epsilon)))
    (t (< (relative-error float1 float2) (* 2.0 long-float-epsilon)))))

(defun float-equal (float1 float2 &optional (epsilon *epsilon*))
  "Return true if the relative error between float1 and float2 is less
than some epsilon."
  (when (and (floatp float1) (floatp float2))
    (%float-equal float1 float2 epsilon)))

(defmacro assert-float-equal (expected form &rest extras)
  (expand-assert :equal form form expected extras :test #'float-equal))

;;; (COMPLEX-EQUAL complex1 complex2 &optional epsilon) => true or false
(defun %complex-equal (complex1 complex2 epsilon)
  "Internal version that does not verify arguments as (complex float)."
  (and
   (%float-equal (realpart complex1) (realpart complex2) epsilon)
   (%float-equal (imagpart complex1) (imagpart complex2) epsilon)))

(defun complex-equal (complex1 complex2 &optional (epsilon *epsilon*))
  "Return true if the relative error between Re(complex1),
Re(complex2) and between Im(complex1), Im(complex2) are each less than
epsilon."
  (cond
    ((and (typep complex1 '(complex float))
          (typep complex2 '(complex float)))
     (%complex-equal complex1 complex2 epsilon))
    ((and (complexp complex1) (complexp complex2))
     (= complex1 complex2))))

(defmacro assert-complex-equal (expected form &rest extras)
  (expand-assert :equal form form expected extras :test #'complex-equal))

;;; (NUMBER-EQUAL number1 number2) => true or false
(defun number-equal (number1 number2 &optional
                     (epsilon *epsilon*) type-agreement)
  "Return true if the numbers are equal using the appropriate
comparison."
  (and
   (or (not type-agreement) (eq (type-of number1) (type-of number2)))
   (%complex-equal
    (coerce number1 '(complex double-float))
    (coerce number2 '(complex double-float))
    epsilon)))

(defmacro assert-number-equal (expected form &rest extras)
  (expand-assert :equal form form expected extras :test #'number-equal))

;;; (NORMALIZE-FLOAT significand &optional exponent) => significand,exponent
;;; [NumAlgoC] : Definition 1.7, pg. 4
;;;
;;; To avoid using 0.1, first 1.0 <= significand < 10. On the final
;;; return, scale 0.1 <= significand < 1.
(defun normalize-float (significand &optional (exponent 0))
  "Return the normalized floating point number and exponent."
  (cond
    ((zerop significand)
     (values significand 0))
    ((>= (abs significand) 10)
     (normalize-float (/ significand 10.0) (1+ exponent)))
    ((< (abs significand) 1)
     (normalize-float (* significand 10.0) (1- exponent)))
    (t (values (/ significand 10.0) (1+ exponent)))))

;;; (SIGFIG-EQUAL float1 float2 significant-figures) => true or false
(defun sigfig-equal (float1 float2 &optional (significant-figures *significant-figures*))
  "Return true if the floating point numbers have equal significant
figures."
  ;; Convert 0.5 to precision of FLOAT1 and 10 to precision of FLOAT2.
  ;; Then, rely on Rule of Float and Rational Contagion, CLHS 12.1.4.1,
  ;; to obtain a DELTA of the proper precision.
  (let ((delta (* (float 0.5 float1) (expt (float 10 float2) (- significant-figures)))))
    (if (or (zerop float1) (zerop float2))
	(< (abs (+ float1 float2)) delta)
	(multiple-value-bind (sig1 exp1) (normalize-float float1)
	  (multiple-value-bind (sig2 exp2) (normalize-float float2)
	    (and (= exp1 exp2)
		 (< (abs (- sig1 sig2)) delta)))))))

(defmacro assert-sigfig-equal (expected form &rest extras)
  (expand-assert :equal form form expected extras :test #'sigfig-equal))

;;; (NUMERICAL-EQUAL result1 result2) => true or false
;;;
;;; This is a universal wrapper created by Liam Healy. It is
;;; implemented to support testing in GSLL. The interface is expanded,
;;; but backwards compatible with previous versions.
;;;
(defgeneric numerical-equal (result1 result2 &key test)
  (:documentation
   "Return true if the results are numerically equal according to :TEST."))

(defmethod numerical-equal ((result1 number) (result2 number)
			    &key (test #'number-equal))
  "Return true if the the numbers are equal according to :TEST."
  (funcall test result1 result2))

(defun %sequence-equal (seq1 seq2 test)
  "Return true if the sequences are equal in length and each element
is equal according to :TEST."
  (when (= (length seq1) (length seq2))
    (every (lambda (s1 s2) (numerical-equal s1 s2 :test test))
	   seq1 seq2)))

(defmethod numerical-equal ((result1 list) (result2 list)
			    &key (test #'number-equal))
  "Return true if the lists are equal in length and each element is
equal according to :TEST."
  (%sequence-equal result1 result2 test))

(defmethod numerical-equal ((result1 vector) (result2 vector)
			    &key (test #'number-equal))
  "Return true if the vectors are equal in length and each element is
equal according to :TEST."
  (%sequence-equal result1 result2 test))

(defmethod numerical-equal ((result1 list) (result2 vector)
			    &key (test #'number-equal))
  "Return true if every element of the list is equal to the
corresponding element of the vector."
  (%sequence-equal result1 result2 test))

(defmethod numerical-equal ((result1 vector) (result2 list)
			    &key (test #'number-equal))
  "Return true if every element of the list is equla to the
corresponding element of the vector."
  (%sequence-equal result1 result2 test))

(defmethod numerical-equal ((result1 array) (result2 array)
			    &key (test #'number-equal))
  "Return true if the arrays are equal in dimension and each element
is equal according to :TEST."
  (when (equal (array-dimensions result1) (array-dimensions result2))
    (every test
	   (make-array (array-total-size result1)
		       :element-type (array-element-type result1)
		       :displaced-to result1)
	   (make-array (array-total-size result2)
		       :element-type (array-element-type result2)
		       :displaced-to result2))))

(defmacro assert-numerical-equal (expected form &rest extras)
  (expand-assert :equal form form expected extras :test #'numerical-equal))

;;; Diagnostic functions
;;; Failing a unit test is only half the problem.

;;; Relative floating point error.
(defun float-error (float1 float2)
  "Return the relative error of the floating point numbers."
  (if (and (floatp float1) (floatp float2))
      (relative-error float1 float2)
      (error "~A and ~A are not float." float1 float2)))

;;; Floating point error as a multiple of epsilon.
(defun %float-error-epsilon (float1 float2 epsilon)
  "Return the relative error divided by epsilon."
  (cond
    (epsilon
     (/ (relative-error float1 float2) epsilon))
    ((or (typep float1 'short-float) (typep float2 'short-float))
     (/ (relative-error float1 float2) short-float-epsilon))
    ((or (typep float1 'single-float) (typep float2 'single-float))
     (/ (relative-error float1 float2) single-float-epsilon))
    ((or (typep float1 'double-float) (typep float2 'double-float))
     (/ (relative-error float1 float2) double-float-epsilon))
    (t (/ (relative-error float1 float2) long-float-epsilon))))

(defun float-error-epsilon (float1 float2 &optional (epsilon *epsilon*))
  "Return the relative error divided by epsilon."
  (if (and (floatp float1) (floatp float2))
      (%float-error-epsilon float1 float2 epsilon)
      (error "~A and ~A are not float." float1 float2)))

;;; Relative complex floating point error.
(defun %complex-error (complex1 complex2)
  "Internal version with no type checking."
  (complex
   (relative-error (realpart complex1) (realpart complex2))
   (relative-error (imagpart complex1) (imagpart complex2))))

(defun complex-error (complex1 complex2)
  "Return the relative error of each component of the complex numbers."
  (if (and (typep complex1 '(complex float))
	   (typep complex2 '(complex float)))
      (%complex-error complex1 complex2)
      (error "~A and ~A are not complex float." complex1 complex2)))

;;; Complex floating point error as a multiple of epsilon.
(defun %complex-error-epsilon (complex1 complex2 epsilon)
  "Return the relative error of each component divided by epsilon."
  (complex
   (%float-error-epsilon (realpart complex1) (realpart complex2) epsilon)
   (%float-error-epsilon (imagpart complex1) (imagpart complex2) epsilon)))

(defun complex-error-epsilon (complex1 complex2 &optional (epsilon *epsilon*))
  "Return the relative error of each component divided by epsilon."
  (if (and (typep complex1 '(complex float))
	   (typep complex2 '(complex float)))
      (%complex-error-epsilon complex1 complex2 epsilon)
      (error "~A and ~A are not complex float." complex1 complex2)))

;;; Numeric relative error
(defun number-error (number1 number2)
  "Return the relative error of the number."
  (cond
    ((and (floatp number1) (floatp number2))
     (relative-error number1 number2))
    ((and (typep number1 '(complex float)) (typep number2 '(complex float)))
     (%complex-error number1 number2))
    ((and (numberp number1) (numberp number2))
     (abs (- number1 number2)))
    (t (error "~A and ~A are not numbers." number1 number2))))

;;; Numeric error as a multiple of epsilon.
(defun number-error-epsilon (number1 number2 &optional (epsilon *epsilon*))
  "Return the relative error divided by epsilon."
  (cond
    ((and (floatp number1) (floatp number2))
     (%float-error-epsilon number1 number2 epsilon))
    ((and (typep number1 '(complex float)) (typep number2 '(complex float)))
     (%complex-error-epsilon number1 number2 epsilon))
    (t (error "~A and ~A are not float or complex float numbers." number1 number2))))

;;; Sequence errors and the indices.
(defun %sequence-error (sequence1 sequence2 test error-function)
  "Return a sequence of the indice and error between the sequences."
  (let ((n1 nil) (n2 nil)
	(errseq '()))
    (dotimes (index (length sequence1) errseq)
      (setf n1 (elt sequence1 index)
	    n2 (elt sequence2 index))
      (unless (funcall test n1 n2)
	(push (list (1- index) n1 n2 (funcall error-function n1 n2))
	      errseq)))))

(defun sequence-error (sequence1 sequence2 &key
		       (test #'number-equal)
		       (error-function #'number-error))
  "Return a sequence of the indice and error between the sequence elements."
  (cond
    ((not (typep sequence1 'sequence))
     (error "SEQUENCE1 is not a valid sequence."))
    ((not (typep sequence2 'sequence))
     (error "SEQUENCE2 is not a valid sequence."))
    ((not (= (length sequence1) (length sequence2)))
     (error "Lengths not equal. SEQUENCE1(~D) /= SEQUENCE2(~D)."
	    (length sequence1) (length sequence2)))
    (t (%sequence-error sequence1 sequence2 test error-function))))

;;; Array errors and the indices.
(defun %array-indices (row-major-index dimensions)
  "Recursively calculate the indices from the row major index."
  (let ((remaining (rest dimensions)))
    (if remaining
	(multiple-value-bind (index remainder)
	    (floor row-major-index (reduce #'* remaining))
	  (cons index (%array-indices remainder remaining)))
	(cons row-major-index nil))))

(defun %array-error (array1 array2 test errfun)
  "Return a list of the indices, values and error of the elements that
are not equal."
  (let ((dimensions (array-dimensions array1))
	(n1 nil) (n2 nil)
	(indices '())
	(errseq '()))
    (dotimes (index (array-total-size array1) errseq)
      (setf indices (%array-indices index dimensions)
	    n1 (apply #'aref array1 indices)
	    n2 (apply #'aref array2 indices))
      (unless (funcall test n1 n2)
	(push (list indices n1 n2 (funcall errfun n1 n2))
	      errseq)))))

(defun array-error (array1 array2 &key
		    (test #'number-equal)
		    (error-function #'number-error))
  "Return a list of the indices and error between the array elements."
  (cond
    ((not (arrayp array1))
     (error "ARRAY1 is not an array."))
    ((not (arrayp array2))
     (error "ARRAY2 is not an array."))
    ((not (equal (array-dimensions array1) (array-dimensions array2)))
     (error "Arrays are not equal dimensions."))
    (t (%array-error array1 array2 test error-function))))

;;; Floating point data functions
(defun make-2d-list (rows columns &key (initial-element 0))
  "Return a nested list with INITIAL-ELEMENT."
  (mapcar (lambda (x) (make-list columns :initial-element x))
          (make-list rows :initial-element initial-element)))

(defun %complex-float-random (limit &optional (state *random-state*))
  "Return a random complex float number."
  (complex
   (random (realpart limit) state)
   (random (imagpart limit) state)))

(defun %complex-rational-random (limit &optional (state *random-state*))
  "Return a random complex rational number."
  (let ((imaglimit (imagpart limit)))
    (if (< 1 imaglimit)
        (complex
         (random (realpart limit) state)
         ;; Ensure that the imaginary part is not zero.
         (do ((im (random imaglimit state)
                  (random imaglimit state)))
             ((< 0 im) im)))
        (error "Imaginary part must be greater than 1."))))

(defun complex-random (limit &optional (state *random-state*))
  "Return a random complex number. "
  (check-type limit complex)
  (if (typep limit '(complex rational))
      (%complex-rational-random limit state)
      (%complex-float-random limit state)))

(defun make-random-list (size &optional (limit 1.0))
  "Return a list of random numbers."
  (mapcar (if (complexp limit) #'complex-random #'random)
	  (make-list size :initial-element limit)))

(defun make-random-2d-list (rows columns &optional (limit 1.0))
  "Return a nested list of random numbers."
  (mapcar (lambda (x) (make-random-list columns x))
          (make-list rows :initial-element limit)))

(defun make-random-2d-array (rows columns &optional (limit 1.0))
  "Return a 2D array of random numbers."
  (let ((new-array (make-array (list rows columns)
			       :element-type (type-of limit)))
	(random-func (if (complexp limit)
			 #'complex-random
			 #'random)))
    (dotimes (i0 rows new-array)
      (dotimes (i1 columns)
	(setf (aref new-array i0 i1)
	      (funcall random-func limit))))))
