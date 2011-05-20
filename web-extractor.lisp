(in-package :web-extractor)


;;;; SOME THIRD PARTY LIBS TESTS    

;; (defun extract () 1)

;; ;; (pprint
;; ;;  (parse-html 
;; ;;   (http-request "http://www.google.com")))

;; (setq *atp-ranking* (drakma:http-request "http://www.atpworldtour.com/Rankings/Singles.aspx" :user-agent :explorer))

;; (define-sanitize-mode links
;;     :elements (:a) 
;;     :attributes ((:a :href)))

;; (setq *atp-ranking-links* (parse-html (clean *atp-ranking* links)))


;; UTILS
(defun get-key (symbol list)
  (when (listp list)
    (if (eq (car list) symbol)
	(car (cdr list))
	(get-key symbol (cdr list)))))

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

;; THE REAL THING


;; (def-web-extractor match-detail
;;     ((against :finder #'against-finder)))
;;
;;(defparameter match-detail (list (list 'against #'against-finder)))


(defmacro def-web-extractor (name attributes) 
  `(defparameter ,name 
     (quote
      ,(loop for attr in attributes 
	  collect 
	    (list (car attr) (get-key :finder attr))))))
	    
		      
(defun extract (data struct-map)
  (loop for attr in struct-map collect
       (list
	(car attr)
	(cond 
	  ((get-key :follow attr) (extract 
				   (funcall (get-key :finder attr) data) 
				   (get-key :follow attr))
	   ((get-key :collection attr) (let ((splitted-list (funcall (get-key :splitter attr) data)))
					 (loop for spl in splitted-list collect
					      (extract
					       spl
					       (get-key :collection attr)))))
	   (t (funcall (get-key :finder attr) data)))
	  

;; HOW TO USE IT AGAINST something like test.html

(defparameter *test-data* (file-string "test.html"))


(def-web-extractor match-detail
    ((against :finder #'against-finder)))

(def-web-extractor match-map
    ((date :finder #'date-finder)
     (score :finder #'score-finder)
     (details :follow match-detail :finder #'match-detail-finder))) 

(def-web-extractor simple-player-map 
    ((name :finder #'(lambda (html-str)
		       (regexp-finder "Name:</td><td>(.*)</td>")))
     (age :finder #'(lambda (html-str)
		       (regexp-finder "Age:</td><td>(.*)</td>")))))

(def-web-extractor player-map 
    ((name :finder #'(lambda (html-str)
		       (regexp-finder "Name:</td><td>(.*)</td>")))
     (age :finder #'(lambda (html-str)
		       (regexp-finder "Age:</td><td>(.*)</td>")))
     (matches :collection match-map 
	      :splitter #'(lambda (html-str)
			   (xpath-splitter "/" html-str)))))

(def-web-extractor page-map
    ((players :collection player-map 
	      :splitter #'players-splitter)))

(setf *player-juan-web-ex* (extract "string from test.html" page-map))

(funcall *player-juan-web-ex*)

;; ((:name "Juan Monetta")
;;  (:age "27")
;;  (:matches 
;;   (:COLLECTION 
;;    ((:date "20/10/1990")
;;     (:score "2-0")
;;     (:details 
;;      (:against "Alejandro")))
;;    ((:date "20/10/1991")
;;     (:score "2-1")
;;     (:details 
;;      (:against "Pepe"))))))



(expose players-web-ex)

;; You should GET the data via HTTP using REST like services :
;;
;; http://localhost/player
;; http://localhost/player/1
;; http://localhost/player/1/age
;; http://localhost/player/1/matches/
;; http://localhost/player/1/matches/1
;; http://localhost/player/1/matches/1/date
;; http://localhost/player/1/matches/1/details/against
