(in-package :web-extractor)

(declaim (optimize (debug 3)))

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
	(declare (optimize
		(debug 3)
		(speed 0)
		(space 0)
		(compilation-speed 0)
		(safety 3)))
  (lambda (html-str)
    (let ((res (register-groups-bind (first) ((create-scanner regexp :single-line-mode t)
					      html-str) first)))
      (when debug (print (format nil "regexp-finder regexp : ~a~% OUTPUT : ~a" regexp res)))
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
	   (debug-patch (when debug (print (format nil "xpath-splitter INPUT : ~%~a" clean-xhtml))))
	   (res (when (not (equal clean-xhtml nil)) 
		  (with-parse-document (doc clean-xhtml)
		  (iter (for node in-xpath-result xpath-expr on doc)
			(let ((node-str (remove-nl-tab-spc (serialize node :to-string))))
			  (when (> (length node-str) 0) (collect node-str))))))))
      (when debug (print (format nil "xpath-splitter OUTPUT : ~%~a" res)))
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
		     (try-cast-type (getf properties :try-cast-type))
		     (follow-type (getf properties :follow))
		     (http-method (getf properties :http-method))
		     (post-parameters-gen (getf properties :post-parameters-gen))
		     (col-item-type (getf properties :collection))
		     (next-page-gen (getf properties :next-page-gen))
		     (skip-follow-p (getf properties :skip-follow-p))
		     (col-limit (if (getf properties :limit) (getf properties :limit) 10))
		     (splitter (getf properties :splitter)))
		(cond
		  ((member :follow properties)
		   `(list (quote ,name)
			  :finder ,finder
			  :skip-follow-p ,skip-follow-p
			  :http-method ,http-method
			  :post-parameters-gen ,post-parameters-gen
			  :follow ,follow-type))
		  ((member :collection properties)
		   `(list (quote ,name) 
			  :collection ,col-item-type
			  :splitter ,splitter
			  :next-page-gen ,next-page-gen
			  :http-method ,http-method
			  :post-parameters-gen ,post-parameters-gen
			  :skip-follow-p ,skip-follow-p
			  :limit ,col-limit))
		  (t 
		   `(list (quote ,name)
			  :finder ,finder
			  :try-cast-type (quote ,(or try-cast-type 'STRING))))))))))

(defun extract-collection (base-url properties data)
  (let* ((col-item-type (getf properties :collection))
	 (next-page-gen (getf properties :next-page-gen))
	 (http-method (getf properties :http-method))
	 (post-parameters-gen (getf properties :post-parameters-gen))
	 (col-limit (getf properties :limit))
	 (splitter (getf properties :splitter))
	 (skip-follow-p (getf properties :skip-follow-p)))
	  (do* ((counter 1)
		(next-page-url nil (when (and col-limit (< counter col-limit) next-page-gen) 
				     (funcall next-page-gen base-url data)))
		(next-page-post-params nil (when post-parameters-gen (funcall post-parameters-gen data)))
		(next-page-data nil (when next-page-url 
				      (clean-for-xpath
				       ;; If we have a skip-follow-predicate and it returns true
				       (if (and skip-follow-p (funcall skip-follow-p next-page-url)) 
					   "" ;; Don't follow the link, just return empty string
					   (get-string-from-url next-page-url :method http-method :post-parameters next-page-post-params))))) ;; else go and download
		(splitted-list (funcall splitter data) (when next-page-data 
							 (funcall splitter next-page-data)))
		(collection '()))
	       ((or ;; Finish condition
		 (eq splitted-list nil) ;; Or we ran out of elements
		 (and col-limit (>= counter col-limit))) ;; Or we hit our limit
		collection)
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
						     (incf counter)))))))
  
(defun extract-follow (base-url properties data)
  (let* ((skip-follow-p (getf properties :skip-follow-p))
	 (finder (getf properties :finder))
	 (follow-type (getf properties :follow))
	 (follow-url (render-uri (merge-uris 
				  (parse-uri (funcall finder data))
				  (parse-uri base-url)) 
				 nil))
	 (http-method (getf properties :http-method))
	 (post-parameters-gen (getf properties :post-parameters-gen))
	 (post-parameters (when post-parameters-gen (funcall post-parameters-gen data))))  


    ;; If we have a skip-follow-predicate and it returns true
    (if (and skip-follow-p (funcall skip-follow-p follow-url)) 
	"" ;; Don't follow the link, just return empty string
	(extract  ;; Otherwise go for the link
	 :url follow-url
	 :struct-map follow-type :http-method http-method :post-parameters post-parameters))))

(defun extract-simple (base-url properties data struct-map)
  (declare (ignore base-url struct-map))
  (let ((finder (getf properties :finder))
	(try-cast-type (getf properties :try-cast-type)))
    (cond ((and (eq try-cast-type 'BOOLEAN) (funcall finder data)) t)
	  ((eq try-cast-type 'NUMBER) (read-from-string (or (funcall finder data) "0")))
	  (t (string-trim " " (funcall finder data))))))
    
(defvar *page-cache* (make-hash-table :test 'equal))

(defun extract (&key str url struct-map http-method post-parameters)
  (let* ((data (cond ((not (equal str nil)) str) ;; If we have an string, lets use that as data
		    (t (or ;; If don't 
			(gethash url *page-cache*) ;; Let's try to retrieve that from our cache based on URL
			  ;; We didn't find anything in our cache
			    (setf (gethash url *page-cache*) ;; Go for it and store it on the cache
				  (clean-for-xpath (get-string-from-url url :method http-method :post-parameters post-parameters))))))))
     (loop for attr in struct-map collect
	 (let* ((name (car attr))
		(properties (cdr attr)))
	   (cons
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
