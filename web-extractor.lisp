(in-package :web-extractor)

;; Some examples are :

;; (def-web-extractor phone-webe
;;     ((number :finder phone-finder-func)))

;; (def-web-extractor person-webe
;;     ((name :finder name-finder-func)
;;      (age :finder age-finder-func)
;;      (phones :follow phone-webe :finder phone-link-finder-func)))

;; (def-web-extractor persons-webe
;;     ((persons :collection person-webe
;; 		  :splitter persons-splitter-func
;; 		  :limit 10
;; 		  :next-page-gen persons-nex-page-gen-func)))

;; To the :finder indicator you should give any function that should recieve 
;; context HTML data and the returned value will be used as an attribute value.

;; To the :follow indicator you should give a web-extractor that will extract 
;; the data from the result of following the link returned by the :finder function

;; With the :collection indicator you specify that the attribute will contain a collection
;; of the web-extractor you put after. Some indicators you can use in a :collection are :
;; :splitter A function that will recieve context HTML and should return a list of HTML subparts
;; :limit A number that limits the collection lenght
;; :next-page-gen A function that should recieve base-url and context HTML and should return
;;                the next page to visit to continue downloading the collection.

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
       (declare (ignore html-data))
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
	      (t (extract-simple url properties data struct-map))))))))



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


