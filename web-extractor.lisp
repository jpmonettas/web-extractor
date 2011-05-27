(in-package :web-extractor)


;;;; SOME THIRD PARTY LIBS TESTS    

;; (pprint
;;  (parse-html 
;;   (http-request "http://www.google.com")))

;; (define-sanitize-mode links
;;     :elements (:a) 
;;     :attributes ((:a :href)))

;; (setq *atp-ranking-links* (parse-html (clean *atp-ranking* links)))



;; THE REAL THING

(defun regexp-finder (regexp)
  (lambda (html-str)
    (register-groups-bind (first) (regexp html-str) first)))

(defun xpath-finder (xpath-expr) 
  (lambda (html-str)
    (with-parse-document (doc html-str)
      (xpath:find-string doc xpath-expr))))
  
(defun regexp-splitter (regexp) nil)

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
		    (follow (getf properties :follow))
		    (collection (getf properties :collection))
		    (splitter (getf properties :splitter)))
	       (cond
		 ((member :follow properties)
		  `(list (quote ,name)
			 :finder ,finder
			 :follow ,follow))
		 ((member :collection properties)
		  `(list (quote ,name) 
			 :collection ,collection
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
	      (splitter (getf properties :splitter)))
	 (list
	  name
	  (cond 
	    ((member :follow properties)                            
	     (extract 
	      :url (funcall finder data)
	      :struct-map follow-type))
	    ((member :collection properties)                        
	     (cons :COLLECTION
		   (let ((splitted-list (funcall splitter data)))
		     (if col-item-type
			 (loop for item in splitted-list collect
			      (extract
			       :str item
			       :struct-map col-item-type))
			 splitted-list))))
	    (t 
	     (funcall finder data))))))))
  


;; (expose players-web-ex)

;; You should GET the data via HTTP using REST like services :
;;
;; http://localhost/player
;; http://localhost/player/1
;; http://localhost/player/1/age
;; http://localhost/player/1/matches/
;; http://localhost/player/1/matches/1
;; http://localhost/player/1/matches/1/date
;; http://localhost/player/1/matches/1/details/against
