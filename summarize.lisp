;;;-*- Mode: Lisp; Syntax: ANSI-Common-Lisp -*-
(in-package :lisp-unit)
(cl-interpol:enable-interpol-syntax)

(defun with-summary-context (body-fn &aux (*print-pretty* t) rtn)
  (pprint-logical-block (*test-stream* nil)
    (handler-bind
        ((test-start
           (lambda (c) (format *test-stream* "~@:_Starting: ~A" (name (unit-test c)))))
         (all-tests-complete (lambda (c) (%print-result-summary (results c))))
         (test-complete (lambda (c) (%print-summary (result c)))))
      (setf rtn (multiple-value-list (funcall body-fn)))))
  (apply #'values rtn))

(defmacro with-summary (() &body body)
  `(with-summary-context (lambda () ,@body)))

(defparameter *status-prefix* "~@:_  |  ")

(defun %out (s &rest args
               &aux (prefix (alexandria:ensure-list *status-prefix*))
               (*print-pretty* t))
  (format *test-stream* "~?~?" (first prefix) (rest prefix) s args))

(defun %print-result-summary (o)
  (let ((total (length (test-names o)))
        (passed (length (passed-assertions o)))
        (failed (length (failed-assertions o)))
        (errors (length (errors o)))
        (warnings (length (all-warnings o)))
        (empty (length (empty o)))
        (missing (length (missing o))))
    (format *test-stream* "~@:_Test Summary (~D tests ~D sec)~@:_" total (run-time o))
    (format *test-stream* "  | ~D assertions total~@:_" (+ passed failed))
    (format *test-stream* "  | ~D passed~@:_" passed)
    (format *test-stream* "  | ~D failed~@:_" failed)
    (format *test-stream* "  | ~D execution errors~@:_" errors)
    (format *test-stream* "  | ~D warnings~@:_" warnings)
    (format *test-stream* "  | ~D empty~@:_" empty)
    (format *test-stream* "  | ~D missing tests~@:_" missing)))

(defun print-summary (o &aux (*print-pretty* t) rtn)
  (pprint-logical-block (*test-stream* nil)
    (setf rtn(multiple-value-list (%print-summary o))))
  (apply #'values rtn))

(defgeneric %print-summary (o)
  (:documentation "Print a summary of all results to the stream.")
  (:method ((o test-results-db))
    (pprint-logical-block (*test-stream* nil)
      (iter (for res in-vector (results o))
        (%print-summary res))
      (%print-result-summary o))
    o)
  (:method  ((run test-result))
    (format *test-stream* "~@:_~A - ~A (~ds) : ~S assertions passed"
            (name (unit-test run)) (status run)
            (- (end-time run) (start-time run))
            (length (passed run)))
    (iter (for s in '(errors failed warnings))
      (print-status-summary run s))
    (format *test-stream* "~@:_")
    run)
  (:method :around ((result failure-result))
    (%out "Failed Form: ~S" (form result))
    (call-next-method)
    (when (extras result)
      (iter (for e in (extras result))
        (%out "~S => ~S")))
    (%out "")
    result)

  (:method ((result failure-result)
            &aux (prefix (alexandria:ensure-list *status-prefix*)))
    (%out "Expected ~{~S~^; ~} " (expected result))
    (format *test-stream*
            "~<~?~:;but saw ~{~S~^; ~}~>"
            (first prefix) (rest prefix)
            (actual result)))
  (:method ((result error-result))
    (%out "~@[Should have signalled ~{~S~^; ~} but saw~]"
          (expected result))
    (%out "~{~S~^; ~}" (actual result))
    result)

  (:method ((result macro-result)
            &aux (prefix (alexandria:ensure-list *status-prefix*)))
    (%out "Should have expanded to ~{~S~^; ~} " (expected result))
    (format *test-stream*
            "~<~?~:;but saw ~{~S~^; ~}~>"
            (first prefix) (rest prefix)
            (actual result))
    result)

  (:method ((result output-result)
            &aux (prefix (alexandria:ensure-list *status-prefix*)))
    (format *test-stream* "~@:_| Should have printed ~{~S~^; ~} "
            (expected result))
    (format *test-stream* "~<~?~:;but saw ~{~S~^; ~}~>"
            (first prefix) (rest prefix)
            (actual result))
    result)
  (:method ((w warning))
    (%out "WARNING: ~A" w)
    (%out "~S" w)
    (%out "")
    w)
  (:method ((e error))
    (%out "ERROR: ~A" e)
    (%out "~S" e)
    (%out "")
    e))

(defgeneric print-status-summary (object status)
  (:method ((db test-results-db) s &aux (objs (funcall s db)))
    (when objs
      (iter (for o in objs)
        (print-status-summary o s))))
  (:method ((o test-result) s &aux (objs (funcall s o)))
    (when objs
      (%out "~A (~D)" s (length objs))
      (iter (for o in objs)
        (%print-summary o))
      (%out "---------------------------"))
    ))