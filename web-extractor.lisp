(in-package :web-extractor)

(defun regexp-finder (regexp)
  (lambda (html-str)
    (register-groups-bind (first) ((create-scanner regexp :single-line-mode t) html-str) first)))

(defun xpath-finder (xpath-expr) 
  (lambda (html-str)
    (with-parse-document (doc html-str)
      (serialize 
       (xpath:find-single-node doc xpath-expr)
       :to-string))))

(defun regexp-splitter (regexp)
  (lambda (html-str)
    (let ((collection nil))
      (do-matches-as-strings (item (create-scanner regexp :single-line-mode t) html-str)
	(when (> (length (remove-nl-tab-spc item)) 0)
	  (push 
	   (funcall (regexp-finder regexp) item)  
	    collection)))
      (reverse collection))))
      

(defun xpath-splitter (xpath-expr)
  (lambda (html-str)
    (with-parse-document (doc html-str)
      (iter (for node in-xpath-result xpath-expr on doc)
	    (let ((node-str (remove-nl-tab-spc (serialize node :to-string))))
	      (when (> (length node-str) 0) (collect node-str)))))))

(defun clean-for-xpath (html)
  (let ((xhtml (html2xhtml html)))
    (regex-replace-all "<[Hh][Tt][Mm][Ll].*?>" xhtml "<html>")))

(defmacro def-web-extractor (name attributes) 
  `(defparameter ,name 
     (list
      ,@(loop for attr in attributes collect
	     (let* ((name (car attr))
		    (properties (cdr attr))
		    (finder (getf properties :finder))
		    (follow-type (getf properties :follow))
		    (col-item-type (getf properties :collection))
		    (splitter (getf properties :splitter)))
	       (cond
		 ((member :follow properties)
		  `(list (quote ,name)
			 :finder ,finder
			 :follow ,follow-type))
		 ((member :collection properties)
		  `(list (quote ,name) 
			 :collection ,col-item-type
			 :splitter ,splitter))
		 (t 
		  `(list (quote ,name)
			 :finder ,finder))))))))

  
(defun extract (&key str url struct-map)
  (let ((data (if str 
		  str
		  (clean-for-xpath (get-string-from-url url)))))
  (loop for attr in struct-map collect
       (let* ((name (car attr))
	      (properties (cdr attr))
	      (finder (getf properties :finder))
	      (follow-type (getf properties :follow))
	      (col-item-type (getf properties :collection))
	      (col-limit (if (getf properties :limit) (getf properties :limit) 10))
	      (splitter (getf properties :splitter)))
	 (list
	  name
	  (cond 
	    ((member :follow properties)
	     (let ((follow-url (merge-uris 
				(parse-uri (funcall finder data))
				(parse-uri url))))
	       (extract 
		:url (render-uri follow-url nil)
		:struct-map follow-type)))
	    ((member :collection properties)                        
	     (cons :COLLECTION
		   (let ((splitted-list (funcall splitter data)))
		     (if col-item-type
			 (loop 
			    for item in splitted-list 
			    for i from 1 to col-limit
			    collect
			      (extract
			       :str item
			       :url url
			       :struct-map col-item-type))
			 splitted-list))))
	    (t 
	     (funcall finder data))))))))
  

;;;; SOME IDEAS TO IMPLEMENT
;;
;; - Remove clean-for-xpath, maybe it's better to add <root>...</root> always
;; - Implement :find-pager COLLECTIONS
;; - Implement regexp-splitter
;; - Implement :union-follow
;;
;;;; SOME IDEAS TO INVESTIGATE OR THINK ABOUT
;;
;; - filters or preprocessors maybe based on sanitize::clean
;; - exporting data, REST? RDBMS?




;;;; SOME THIRD PARTY LIBS TESTS    

;; (pprint
;;  (parse-html 
;;   (http-request "http://www.google.com")))

;; (define-sanitize-mode links
;;     :elements (:a) 
;;     :attributes ((:a :href)))

;; (setq *atp-ranking-links* (parse-html (clean *atp-ranking* links)))


