(in-package :web-extractor)



;; HOW TO USE IT AGAINST something like test.html

(def-web-struct-map match-detail
    ((against :finder #'against-finder)))

(def-web-struct-map match-map
    ((date :finder #'date-finder)
     (score :finder #'score-finder)
     (details :follow match-detail :finder #'match-detail-finder))) 

(def-web-struct-map player-map 
    ((name :finder #'(lambda (html-str)
		       (regexp-finder "Name:</td><td>(.*)</td>")))
     (age :finder #'age-finder)
     (matches :collection match-map 
	      :splitter #'(lambda (html-str)
			   (xpath-splitter "" html-str)))))

(def-web-struct-map page-map
    ((players :collection player-map 
	      :splitter #'players-splitter)))

(setf players-web-ex (create-web-ex :url "URL-Players" page-map))

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

 


