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
  (and (= (length element) 2)
       (atom (first element))
       (atom (second element))))

;; This little utility is for grabbing a tree like (A (B "B") (C (D "D"))) and
;; returnin ("B" "D")
(defun tree-to-string-list (tree)
  (cond ((or (eq nil tree) (atom tree)) "")
	((attribute-p tree) (second tree))
	(t (let ((result ""))
	  (loop 
	     for elem in tree
	     do (setq result (concatenate 'string result " " (tree-to-string-list elem))))
	  result))))

(defun store-s-expression-to-file (s file-name)
  (with-open-file (str file-name :direction :output)
    (write s :stream str)))

(defun load-s-expression-from-file (file-name)
  (with-open-file (str file-name :direction :input)
    (read str)))
