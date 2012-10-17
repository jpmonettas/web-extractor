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

(defun get-string-from-url (url)
  (print (format nil "Making an HTTP request to : ~a" url))
  (drakma:http-request url :user-agent :explorer))

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
