;;; -*- Lisp -*- mode
(defpackage org.iodb.cl-sails-system
  (:use #:cl #:asdf))

(in-package :org.iodb.cl-sails-system)

(asdf:operate 'asdf:load-op :paren-files)

(defsystem cl-sails
  :description "A lisp and parenscript implementation of Suave sails."
  :version "0.2.0"
  :author "Red Daly <reddaly at gmail>"
  :license "GPL version 2: http://www.gnu.org/licenses/gpl.html"
  :components ((:module "lisp-src"
			:components
			((:file "package")
			 (:file "sails" :depends-on ("package"))))
	       (:module "paren-src"
			:components 
			((:parenscript-file "sails"))
			:depends-on ("lisp-src")))
  :depends-on ("cl-ppcre" "s-xml" "paren-psos" "paren-util" "paren-files" "parenscript" "paren-events")) ;"xml-mop" "parenscript"))

(defsystem cl-sails-tests
  :components ((:module "lisp-tests"
                        :components ((:file "all-tests"))))
  :depends-on ("cl-sails" "stefil"))
