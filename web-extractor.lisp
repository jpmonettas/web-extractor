(in-package :web-extractor)

(defun regexp-finder (regexp)
  (lambda (html-str)
    (register-groups-bind (first) ((create-scanner regexp :single-line-mode t) html-str) first)))

(defun xpath-finder (xpath-expr &key (add-root nil) (only-text 't)) 
  (lambda (html-str)
    (let ((clean-xhtml (if add-root (add-root html-str) html-str)))
      (with-parse-document (doc clean-xhtml)
	(if only-text
	    (xpath:find-string doc xpath-expr)
	    (serialize (xpath:find-single-node doc xpath-expr) :to-string))))))

(defun regexp-splitter (regexp)
  (lambda (html-str)
    (let ((collection nil))
      (do-matches-as-strings (item (create-scanner regexp :single-line-mode t) html-str)
	(when (> (length (remove-nl-tab-spc item)) 0)
	  (push 
	   (funcall (regexp-finder regexp) item)  
	    collection)))
      (reverse collection))))
      

(defun xpath-splitter (xpath-expr &key (add-root nil))
  (lambda (html-str)
    (let ((clean-xhtml (if add-root (add-root html-str) html-str)))
      (with-parse-document (doc clean-xhtml)
	(iter (for node in-xpath-result xpath-expr on doc)
	      (let ((node-str (remove-nl-tab-spc (serialize node :to-string))))
		(when (> (length node-str) 0) (collect node-str))))))))

(defmacro param-pager (params &key init inc)
  `(let ((cont ,init))
     (lambda (url html-data)
       (prog1
	   (concatenate 'string url (regex-replace-all "{}" ,params (write-to-string cont)))
	 (incf cont ,inc)))))
  
(defun add-root (xml)
  (concatenate 'string "<root>" xml "</root>"))

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
		    (next-page-gen (getf properties :next-page-gen))
		    (col-limit (if (getf properties :limit) (getf properties :limit) 10))
		    (splitter (getf properties :splitter)))
	       (cond
		 ((member :follow properties)
		  `(list (quote ,name)
			 :finder ,finder
			 :follow ,follow-type))
		 ((member :collection properties)
		  `(list (quote ,name) 
			 :collection ,col-item-type
			 :splitter ,splitter
			 :next-page-gen ,next-page-gen
			 :limit ,col-limit))
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
	      (next-page-gen (getf properties :next-page-gen))
	      (col-limit (getf properties :limit))
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
		   (do* ((counter 1)
			 (next-page-url nil (when (and (< counter col-limit) next-page-gen) 
					      (funcall next-page-gen url data)))
			 (next-page-data nil (when next-page-url 
					       (clean-for-xpath (get-string-from-url next-page-url))))
			 (splitted-list (funcall splitter data) (when next-page-data 
								  (funcall splitter next-page-data)))  
			 (collection nil))
			((or (eq splitted-list nil) (>= counter col-limit)) collection)
		     (setq collection (append collection (loop 
							    for item in splitted-list 
							    for i from counter to col-limit
							    collect
							      (if col-item-type
								  (extract
								   :str item
								   :url url
								   :struct-map col-item-type)
								  item)
							    do
							      (incf counter)))))))
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


