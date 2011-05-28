(in-package :web-extractor)

(defun compose (f1 f2)
  (lambda (x)
    (funcall f2 (funcall f1 x))))

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