(in-package :cl-sails)

(defclass sail ()
  ()
  (:documentation ""))

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
  (if (null stream)
      `(progn)
      `(defmethod generate-html ((sail ,sail-class-name))
	 (return ,(sail-xml-to-paren-expression stream :view-variable 'sail)))))

(defun generate-sail-definition (sail-stream class-name &key view-class-name view-superclasses)
  (declare (ignore class-name))
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
      (defclass ,view-class-name (,@view-superclasses)
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

(defun slurp-stream-string-stream2 (stream)
  "Return the contents of file as a string."
  (declare (type stream stream)
	   (optimize (speed 3)))
  (with-output-to-string (out)
    (do ((x (read-char stream nil stream) (read-char stream nil stream)))
        ((eq x stream))
      (write-char x out))))

(defun parse-xml (string-or-stream)
  ;; create an artificial root in case it is a forest
  (labels ((name (stp-node)
	     (let ((prefix (stp:namespace-prefix stp-node)))
	       (if (and prefix (> (length prefix) 0))
		   (format nil "~A:~A" prefix (stp:local-name stp-node))
		   (stp:local-name stp-node))))
	   (process-stp-attribute (attrib)
	     (cons (name attrib)
		   (stp:value attrib)))
	   (process-stp-element (elem)
	     (let ((children (remove-if #'(lambda (e) (typep e 'stp:comment)) (stp:list-children elem))))
	       (assert (every #'(lambda (e) (typep e '(or stp:element stp:text))) children))
	       (make-instance 'xml-element
			      :name (name elem)
			      :close-explicit? (and (null (stp:list-attributes elem))
						    (null (stp:list-children elem)))
			      :attributes (stp:map-attributes 'list #'process-stp-attribute elem)
			      :children (mapcar #'process-stp-node children))))
	   (process-stp-node (node)
	     "either an element or text!"
	     (typecase node
	       (stp:element (process-stp-element node))
	       (stp:text (stp:data node))
	       (t (error "Invalid node type in sail!")))))
	       
    (let* ((string (if (stringp string-or-stream) 
		       string-or-stream
		       (slurp-stream-string-stream2 string-or-stream)))
	   (fake-root (stp:first-child
		       (stp:root
			(cxml:parse (format nil "<sail-root xmlns:sails='http://iodb.org/sails'>~A</sail-root>" string)
				    (stp:make-builder))))))
      (stp:map-children 'list #'process-stp-node fake-root))))

(stp:list-attributes (stp:first-child 
      
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

(defun xml-escape-string (str)
  "Escape a string so that it may be unescaped by an xml parser."
  (cl-who:escape-string str))

(defun alist-as-attr-string (alist)
  (format nil "~{~A ~}"
	  (mapcar #'(lambda (attrib-pair)
		      (format nil (format nil "~A=\"~A\""
					  (let ((it (car attrib-pair)))
					    (if (stringp it) (xml-escape-string it) it))
					  (let ((it (cdr attrib-pair)))
					    (if (stringp it) (xml-escape-string it) it)))))

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
                             (list (car attr-pair)
				   "=\""
				   (cdr attr-pair)
				   "\""))
                         (generate-dynamic-attributes sail-elem
						      :view-variable view-variable
						      :gen-id-function gen-id-function))
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


(defpsmacro defsail (sail-name &optional superclasses class-slots &rest options)
  "Syntax sugar for defining sails.
The :html option sets the HTML for the sail view.  This a lisp form which is evaluated.

The :css option sets the CSS for the sail view.  This is a lisp form which is evaluated.

example:
  (defsail thing-sail ()
    ()
    (:css (css-sexp:with-css-output-to-string (s)
            (:body :font-face \"serif\"))))
"
  (let* ((view-class-name
	  (intern
	   (concatenate
	    'string
	    (string sail-name) "-VIEW")))
	 (html-option-rest (rest (find :html options :key #'car)))
	 (css-option-rest (rest (find :css options :key #'car)))
	 (view-superclasses (or (rest (find :view-superclasses options :key #'car))
				'(html-sail-view)))
	 (html (when html-option-rest
		 (eval `(progn ,@html-option-rest))))
	 (css (when css-option-rest
		(eval `(progn ,@css-option-rest))))
	 (options (remove-if #'(lambda (x) (member x '(:html :css)))
			     options :key #'car)))
    `(progn
      (defclass ,sail-name ,(or superclasses '(standard-sail)) ,class-slots ,@options)
;      (merge-into (slot-value ,sail-name 'prototype) -sail.-controller.prototype)
      (defmethod initialize-instance :before ((our-sail ,sail-name))
        (setf (slot-value our-sail :view)
              (make-instance ,view-class-name))
	;(log (+ "Initializing instance of class " ,(string sail-name)))
	;(log our-sail))
	)
      ,(when css
	 `(append-sail-css ,css))
      ,(if html
	   (with-input-from-string (stream html)
	     (cl-sails:generate-sail-definition stream
						view-class-name
						:view-class-name view-class-name
						:view-superclasses view-superclasses))
	   (cl-sails:generate-sail-definition nil
					      view-class-name
					      :view-class-name view-class-name
					      :view-superclasses view-superclasses)))))
	   
      
(defpsmacro register-sail-event-handlers ((sail &optional sail-value)
					  &body handler-descriptions)
  "SAIL is the symbol used for the sail.  If no SAIL-VALUE is supplied, or it is nil, then
we assume SAIL is a variable bound to the relevant SAIL.  Otherwise, SAIL will be bound
to SAIL-VALUE.

Then each handler description is processed.  A handler description looks like this:
   (field-name event-name handler-lambda-list &body handler-body)

Each handler description is turned into an event registration of the event named NAME on the
DOM element denoted by field FIELD_NAME with handler function described by
   (lambda ,HANDLER-LAMBDA-LIST ,@HANDLER-BODY)."
  (let ((sail-var (gensym "sail")))
    `(let ((,sail-var ,(if sail-value sail-value sail)))
       ,@(mapcar #'(lambda (handler-description)
		     (destructuring-bind (field-name event-name handler-lambda-list &body handler-body)
			 handler-description
		       `(register-dom-event-handler (sail-field ,sail-var ,field-name)
						    ,event-name
						    (lambda ,handler-lambda-list ,@handler-body))))
		 handler-descriptions))))

(defpsmacro setf-field ((sail &key (escape? t)) &body place-value-plist)
  "(setf-field (sail &key escape?) [ field | (field slot*)     value]+"
  (with-ps-gensyms (sail-var escape-var)
    `(let ((,sail-var ,sail))
       (setf ,@(loop :for (field value) :on place-value-plist :by #'cddr
		     :collect `(slot-value (sail-field ,sail-var ,field) 'js-global::inner-h-t-m-l)
		     :collect (if escape? `(escape-html ,value) value))))))