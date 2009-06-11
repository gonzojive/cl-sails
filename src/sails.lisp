(in-package :cl-sails)

(defclass sail ()
  ()
  (:documentation ""))

(defun test-parse (stream)
  (s-xml:start-parse-xml
   stream
   (make-instance
    's-xml:xml-parser-state
    :new-element-hook #'(lambda (name attributes seed)
                          seed)
    :finish-element-hook #'(lambda (name attributes parent-seed seed)
                                      seed)
    :text-hook #'(lambda (string seed)
                   seed))))

(defclass xml-element ()
  ((name
    :initarg :name
    :accessor element-name
    :documentation "tag symbol")
   (attributes
    :initarg :attributes
    :accessor element-attributes
    :documentation "alist of attributes.")
   (child-nodes
    :initarg :children  :initform nil
    :accessor element-child-nodes
    :documentation "List of element's children (strings and elements).")
   (close-tag-explicit?
    :initarg :close-explicit? :initarg :close-tag-explicit?
    :initform nil
    :accessor element-close-tag-explicit?
    :documentation "True if close tag was included explicitly in source.")
   )
  (:documentation ""))

(defclass sail-element ()
  ((tag-name
    :initarg :name :accessor element-name)
   (field-name
    :initarg :field :accessor element-field)
   (first?
    :initarg :first? :initform nil :accessor element-first?)
   (last?
    :initarg :last? :initform nil :accessor element-last?)
   (explicit-id
    :initarg :id :initform nil :accessor element-explicit-id)
   (explicit-attributes
    :initarg :attributes :accessor element-attributes)
   (child-nodes
    :initarg :child-nodes :accessor element-child-nodes
    :documentation "Child nodes are either elements or strings.")
   (close-tag-explicit?
    :initarg :close-tag-explicit? :accessor element-close-tag-explicit?))
  (:documentation ""))

(defgeneric generate-dynamic-attributes (sails-elem &key view-variable gen-id-function)
  (:method ((sails-elem sail-element) &key (view-variable 'this) (gen-id-function 'def-field))
    (let* ((field (or (element-field sails-elem)
                      (and (element-first? sails-elem)
                           (element-last? sails-elem)
                           "root")
                      (and (element-last? sails-elem)
                           "last")
                      (and (element-first? sails-elem)
                           "first")))
           (id-expr
            (when field
              `(,gen-id-function
                ,view-variable
                ,field
                (create
                 ,@(when (element-first? sails-elem)
                         (list :first (element-first? sails-elem)))
                 ,@(when (element-last? sails-elem)
                         (list :last (element-last? sails-elem)))
                 ,@(when (element-explicit-id sails-elem)
                     (list :id (element-explicit-id sails-elem))))))))
      (when id-expr
;                 (list (cons 'id "GENERATED-NONSENSE")))
        (list (cons "id" id-expr))))))

(defun sail-stream-to-paren-html-generator (stream sail-class-name)
  `(defmethod generate-html ((sail ,sail-class-name))
    (return ,(sail-xml-to-paren-expression stream :view-variable 'sail))))

(defun generate-sail-definition (sail-stream class-name &key view-class-name)
  (when (null view-class-name)
    (error "NULL VIEW-CLASS-NAME not allowed."))
;    (setf view-class-name (intern (format nil "~A-VIEW" class-name)
;				  (symbol-package class-name))))
  (handler-bind
      ((s-xml:unmatched-namespace-error
        #'(lambda (err)
	    (declare (ignore err))
            (invoke-restart 's-xml::use-package (find-package :cl-sails)))))
    `(progn
      (defclass ,view-class-name (html-sail-view)
	())
      ,(sail-stream-to-paren-html-generator sail-stream view-class-name))))

;; utility
(defun simplify-adds (paren-expr)
  "takes out redundant plus signs in a parenscript expression."
  (cond
    ((not (listp paren-expr))        paren-expr)
    ((not (eql '+ (car paren-expr))) paren-expr)
    ((eql '+ (car paren-expr))
     `(+ ,@(let ((flattened nil))
		(dolist (operand (mapcar #'simplify-adds (rest paren-expr)))
		  (if (and (listp operand) (eql '+ (first operand)))
		      (setf flattened (append (reverse (rest operand)) flattened))
		      (push operand flattened)))
	     (simplify-js-+-arguments (reverse flattened)))))))

(defun simplify-js-+-arguments (args)
  (cond
    ((null args) nil)
    ;; if first two elements of list are strings, merge them together
    ((and (stringp (first args)) (stringp (second args)))
     (simplify-js-+-arguments (cons (concatenate 'string (first args) (second args))
				    (cdr (cdr args)))))
    (t (cons (first args) (simplify-js-+-arguments (rest args))))))

;; this also works but it's maybe not as intuitive
;(defun simplify-js-+-arguments (original-arguments)
;  (reverse
;   (reduce #'(lambda (x y)
;	       (if (and (stringp y) (stringp (car x)))
;		   (cons (concatenate 'string (car x) y) (cdr x))
;		   (cons y x)))
;	   original-arguments
;	   :initial-value nil)))

(defun parse-xml (stream)
  (let ((element-stack nil) (root-elements nil))
    (s-xml:start-parse-xml
     stream
     (make-instance
      's-xml:xml-parser-state
      :new-element-hook
      #'(lambda (name attributes seed)
	  (let ((new-element
		 (make-instance 'xml-element :name name :attributes attributes)))
	    (if element-stack
		(push new-element (element-child-nodes (first element-stack)))
		(push new-element root-elements))
	    (push new-element element-stack))
	  (when (second element-stack)
	    (setf (element-close-tag-explicit? (second element-stack)) t))
	  seed)
      :finish-element-hook
      #'(lambda (name attributes parent-seed seed)
	  (setf (element-child-nodes (first element-stack))
		(reverse (element-child-nodes (first element-stack))))
	  (pop element-stack)
	  seed)
      :text-hook
      #'(lambda (string seed)
	  (setf (element-close-tag-explicit? (first element-stack)) t)
	  (push string (element-child-nodes (first element-stack)))
	  seed)))
    root-elements))

(defun xml-to-sail-element (xml-elem &key first-element? last-element?)
  (labels ((attrib-equal (sym1 sym2)	     (equalp (string sym1) (string sym2)))
	   (attrib-value (name)
	     (cdr (assoc name (element-attributes xml-elem) :test #'attrib-equal))))
    (let ((field-name (or (attrib-value 'field-name)
			  (attrib-value 'field)))
	  (insertion-location (attrib-value 'insertion-location)))
      (make-instance 'sail-element
		     :name (element-name xml-elem)
		     :field field-name
		     :first? first-element?
		     :last? last-element?
		     :id (attrib-value 'id)
		     :attributes
		     (remove-if #'(lambda (attr-pair)
				    (let ((attr (car attr-pair)))
				      (or (attrib-equal 'field-name attr)
					  (attrib-equal 'field attr)
					  (attrib-equal 'id attr)
					  (attrib-equal 'insertion-location attr)
					  )))
				(element-attributes xml-elem))
		     :child-nodes
		     (mapcar #'(lambda (child-node)
				 (typecase child-node
				   (string child-node)
				   (xml-element
				    (xml-to-sail-element child-node))))
			     (element-child-nodes xml-elem))
		     :close-tag-explicit? (element-close-tag-explicit? xml-elem)))))

(defun xml-elements-to-sail (root-elements)
  (let ((first? t))
    (maplist #'(lambda (root-elems-subl)
		 (let ((result (xml-to-sail-element (car root-elems-subl)
						    :first-element? first?
						    :last-element? (null (cdr root-elems-subl)))))
		   (setf first? nil)
		   result))
	     root-elements)))

(defun alist-as-attr-string (alist)
  (format nil "~{~A ~}"
	  (mapcar #'(lambda (attrib-pair)
		      (format nil (format nil "~A=\"~A\""
					  (car attrib-pair)
					  (cdr attrib-pair))))
		  alist)))

;; TODO: this function is long.  break up all the labels.
(defun sail-xml-to-paren-expression (stream
                                     &key
                                     (view-variable 'this)
                                     (gen-id-function 'def-field))
  "Converts a sail XML stream to a Parenscript expression for outputting
the HTML of the sail to a page."
  (labels ((elem-as-paren (sail-elem)
	     (let ((close-explicit? (element-close-tag-explicit? sail-elem)))
             `(+
               "<" ,(string (element-name sail-elem)) " "
               ,(alist-as-attr-string (reverse (element-attributes sail-elem)))
               ,@(mapcan #'(lambda (attr-pair)
                             (list (car attr-pair) "=\"" (cdr attr-pair) "\""))
                         (generate-dynamic-attributes
                          sail-elem :view-variable view-variable :gen-id-function gen-id-function))
               ,(if (not close-explicit?) " />" " >")
               ,@(mapcar #'(lambda (child-node)
                             (typecase child-node
                               (string child-node)
                               (sail-element (elem-as-paren child-node))))
                         (element-child-nodes sail-elem))
               ,@(when close-explicit?
                       (list (format nil "</~A>" (element-name sail-elem))))))))
    (let ((sail-elements (xml-elements-to-sail (parse-xml stream))))
      (simplify-adds
       `(+ ,@(mapcar #'elem-as-paren sail-elements))))))

(defun display-xml-element (elem)
  (typecase elem
    (string (format t "~A~%" elem))
    (sail-element
     (format t "<~A ~A>~%---children---~%"
             (element-name elem)
             (element-attributes elem))
     (dolist (child (element-child-nodes elem)) (display-xml-element child)))))

(defun example (&optional (test-number 1))
  (with-open-file (stream (format nil "./lisp-tests/sail-0~A.html" test-number))
    (handler-bind
        ((s-xml:unmatched-namespace-error
          #'(lambda (err)
              (invoke-restart 's-xml::use-package *package* ))))
      (sail-stream-to-paren-html-generator
       stream (intern (format nil "EXAMPLE-SAIL-~A" test-number))))))
