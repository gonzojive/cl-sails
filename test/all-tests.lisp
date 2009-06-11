(defpackage org.iodb.cl-sails-tests
  (:use common-lisp stefil cl-sails)
  (:export run-tests)
  (:nicknames cl-sails-tests))

(in-package :org.iodb.cl-sails-tests)

(defsuite cl-sails)

(in-suite cl-sails)

(defun run-tests ()
  (cl-sails))

(deftest relative-filename-test ()
  (is (= 1 1)))