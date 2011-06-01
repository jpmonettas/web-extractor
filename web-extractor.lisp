(in-package :web-extractor)

;; Some helpers for the :finder properties 

(defun regexp-finder (regexp &key (debug nil))
  (lambda (html-str)
    (let ((res (register-groups-bind (first) ((create-scanner regexp :single-line-mode t) html-str) first)))
      (when debug (print res))
      res)))

(defun xpath-finder (xpath-expr &key (add-root nil) (only-text 't)) 
  (lambda (html-str)
    (let ((clean-xhtml (if add-root (add-root html-str) html-str)))
      (with-parse-document (doc clean-xhtml)
	(if only-text
	    (xpath:find-string doc xpath-expr)
	    (serialize (xpath:find-single-node doc xpath-expr) :to-string))))))

;; Some helpers for the :splitter properties

(defun regexp-splitter (regexp)
  (lambda (html-str)
    (let ((collection nil))
      (do-matches-as-strings (item (create-scanner regexp :single-line-mode t) html-str)
	(when (> (length (remove-nl-tab-spc item)) 0)
	  (push 
	   (funcall (regexp-finder regexp) item)  
	    collection)))
      (reverse collection))))
      

(defun xpath-splitter (xpath-expr &key (add-root nil) (debug nil))
  (lambda (html-str)
    (let* ((clean-xhtml (if add-root (add-root html-str) html-str))
	   (debug-patch (when debug (print clean-xhtml)))
	   (res (with-parse-document (doc clean-xhtml)
		  (iter (for node in-xpath-result xpath-expr on doc)
			(let ((node-str (remove-nl-tab-spc (serialize node :to-string))))
			  (when (> (length node-str) 0) (collect node-str)))))))
      (when debug (print res))
      res)))
  
;; Some helpers for the :next-page-gen properties

(defmacro param-pager (params &key init inc)
  `(let ((cont ,init))
     (lambda (url html-data)
       (delcare (ignore html-data))
       (prog1
	   (concatenate 'string url (regex-replace-all "\\(\\)" ,params (write-to-string cont)))
	 (incf cont ,inc)))))

;; This are patch functions, expect to remove this asap.
  
(defun add-root (xml)
  (concatenate 'string "<root>" xml "</root>"))

(defun clean-for-xpath (html)
  (let ((xhtml (html2xhtml html)))
    (regex-replace-all "<[Hh][Tt][Mm][Ll].*?>" xhtml "<html>")))

;; Here starts the real thing

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

(defun extract-collection (base-url properties data)
  (let* ((col-item-type (getf properties :collection))
	 (next-page-gen (getf properties :next-page-gen))
	 (col-limit (getf properties :limit))
	 (splitter (getf properties :splitter)))
    (cons :COLLECTION
	  (do* ((counter 1)
		(next-page-url nil (when (and (< counter col-limit) next-page-gen) 
				     (funcall next-page-gen base-url data)))
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
							  :url base-url
							  :struct-map col-item-type)
							 item)
						   do
						     (incf counter))))))))
  
(defun extract-follow (base-url properties data)
  (let* ((finder (getf properties :finder))
	 (follow-type (getf properties :follow))
	 (follow-url (render-uri (merge-uris 
				  (parse-uri (funcall finder data))
				  (parse-uri base-url)) 
				 nil)))
    (extract 
     :url follow-url
     :struct-map follow-type)))

(defun extract-simple (base-url properties data struct-map)
  (declare (ignore base-url struct-map))
  (let ((finder (getf properties :finder)))
    (funcall finder data)))
    
;; This is the main function for scrapping the web normaly based on a base url and
;; a struct-map defined with def-web-extractor. It also accepts as string as the data
;; input instead of a url if you call it with the str param.
;; The output of this function is a list of plist in wich each plist has one of the
;; struct-map attributes name as the property and as the value will contain the extraction
;; for that attribute.  
(defun extract (&key str url struct-map)
  (let ((data (if str 
		  str
		  (clean-for-xpath (get-string-from-url url)))))
    (loop for attr in struct-map collect
	 (let* ((name (car attr))
		(properties (cdr attr)))
	   (list
	    name
	    (cond 
	      ((member :follow properties) (extract-follow url properties data))
	      ((member :collection properties) (extract-collection url properties data))
	      (t (extract-simple url properties data))))))))



;;;; SOME IDEAS TO IMPLEMENT
;;
;; - Remove clean-for-xpath, maybe it's better to add <root>...</root> always
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


