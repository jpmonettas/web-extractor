(in-package :web-extractor)


(defmacro compose (&rest funcs)
  `(lambda (init-param)
     ,(labels ((compose-aux (funcs-list)
			    (cond ((eq funcs-list nil) nil)
				  ((= (length funcs-list) 1) `(funcall ,@funcs-list init-param))
				  (t `(funcall ,(car funcs-list) ,(compose-aux (cdr funcs-list)))))))
	      (compose-aux (reverse funcs)))))

(defun file-string (path)
  (with-open-file (s path)
    (let* ((len (file-length s))
           (data (make-string len)))
      (values data (read-sequence data s)))))

(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))

(defun remove-nl-tab-spc (str)
  (string-trim " " 
	       (remove-if #'(lambda (c) 
			      (or (equal c #\Newline) (equal c #\Tab))) str))) 

(defun collect-items (struc pred)
  "Givean a structure like the returned by extract and a predicate, collect and return
   all the assoc elements where the predicate returns T"
  (declare (optimize (debug 3)))
    (cond ((eq struc nil) nil)
	  ((atom struc) nil)
	  ((consp struc)
	   (if (funcall pred struc)
	       (list struc)
	       (if (listp (cdr struc))
		   (let ((founds '()))
		     (loop for attr in struc do
			  (setq founds (append (collect-items attr pred) founds)))
		     founds)
		   nil)))))

 

(defun with-direct-attribute-value (attr pred)
  "Returns a function that applies over the value of a given attribute of an assoc object.
  It's just a helper for creating predicates for collect-items"
  (lambda (item)
    (when (consp (cdr item))
      (and 
       (assoc attr (cdr item))
       (funcall pred (cdr (assoc attr (cdr item))))))))


(defun get-string-from-url (url &key method post-parameters cookie-jar)
  (when (not method) (setq method :get))
  (print (format nil "Making an HTTP ~a to : ~a" method url))
  (multiple-value-bind (body status) (drakma:http-request url :user-agent :explorer :method method :parameters post-parameters :cookie-jar cookie-jar)
    (when (and (eq method :post) post-parameters)
      (print (format nil "Since it's a POST and we have parameters : ~a" post-parameters)))
    body))

(defun html2xhtml (file &key (if-exists :error))
    (with-open-file (out (make-pathname :type "xml" :defaults file)
			 :element-type '(unsigned-byte 8)
			 :if-exists if-exists
			 :direction :output)
      (chtml:parse (pathname file)
		   (cxml:make-octet-stream-sink out))))

(defun html2xhtml (html)
      (chtml:parse html (cxml:make-string-sink)))


(defun attribute-p (element)
  (and
   (listp element)
   (= (length element) 2)
   (atom (first element))
   (atom (second element))))

;; This little utility is for grabbing a tree like (A (B "B") (C (D "D"))) and
;; returnin (B "B" D "D")
(defun tree-to-property-list (tree)
  (cond ((or (eq nil tree) (atom tree)) nil)
	((attribute-p tree) tree)
	(t (let ((result nil))
	  (loop 
	     for branch-elem in tree
	     do (loop for elem in (tree-to-property-list branch-elem) do (push elem result)))
	  (reverse result)))))

(defun store-s-expression-to-file (s file-name)
  (with-open-file (str file-name :direction :output)
    (write s :stream str)))

(defun load-s-expression-from-file (file-name)
  (with-open-file (str file-name :direction :input)
    (read str)))

(defun get-property-value (property-name tree)
  (cond ((or (eq tree nil) (atom tree)) nil)
	((and 
	  (listp tree)
	  (= (length tree) 2)
	  (eq (first tree) (intern property-name)))
	 (second tree))
	(t 
	 (let ((result nil) )
	   (loop for n in tree do
		(let ((res (get-property-value property-name n)))
		  (if (listp res)
		      (loop for r in res do (push r result))
		      (push res result))))
	   result))))

(defun pprint-assoc-list (alist hide-nils &optional (tab-level 0))
  (flet ((generate-tabs (n) ;; Just returns a string with N tabs contatenated
	   (do ((i 0 (1+ i))
		(result ""))
	       ((= i n) result)
	     (setq result (concatenate 'string (format nil "~T") result)))))
    (if (null alist)
	nil
	(let ((key (car alist))
	      (value (cdr alist)))
	  (cond 
	    ((and (atom key) (atom value)) ;; If it has the form (atom-key . atom-value)
	     (if (not (and hide-nils (null value)))
		 (print (concatenate 'string 
				     (generate-tabs tab-level) 
				     (format nil "(~a . ~a)" 
					     key
					     (if (stringp value)
						 (if (<= (length value) 50)
						     value
						     (concatenate 'string (subseq value 0 50) "..."))
						 value))))))
	    ((and (atom key) (listp value)) ;; If it has the form (atom-key . list-value)
	     (print (concatenate 'string (generate-tabs tab-level) (format nil "(~a . " key)))
	     (loop for e in value do
		  (pprint-assoc-list e hide-nils (+ 2 tab-level)))
	     (print (concatenate 'string (generate-tabs tab-level) ")")))
	  (t (loop for e in alist do ;; Normally if it has the form (list-value list-value)
		  (pprint-assoc-list e hide-nils (+ 2 tab-level)))))))))
