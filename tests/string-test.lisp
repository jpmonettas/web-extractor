(use-package :webe)
;; HOW TO USE IT AGAINST something like test.html

(defparameter *test-data* (file-string "/home/juan/proyectos/web-extractor/test.html"))

(def-web-extractor match-detail
    ((against :finder #'(lambda (html-str) nil))))

(def-web-extractor match-map 
    ((date :finder (regexp-finder "<li>.*(\\d{2}/\\d{2}/\\d{4}).*<a"))
     (score :finder (regexp-finder "<li>.*(\\d{1}-\\d{1}).*<a"))
     (details :follow match-detail :finder (regexp-finder "href=\"(.*)\""))))

(def-web-extractor player-map
    ((name :finder (regexp-finder "Name:</td><td>([A-Za-z ]*)"))
     (age :finder (regexp-finder "Age:</td><td>([0-9]*)"))
     (matches :collection match-map 
	      :splitter (xpath-splitter "/td/table/tr/td/ul/node()"))))

(def-web-extractor page-map
    ((players :collection player-map 
	      :splitter (xpath-splitter "/html/body/table/tr/node()"))))

(extract *test-data* page-map)

;; Should return this
;;
;; ((PLAYERS
;;   (:COLLECTION
;;    ((NAME "Juan Monetta") (AGE "27")
;;     (MATCHES
;;      (:COLLECTION
;;       ((DATE "20/10/1990") (SCORE "2-0")
;;        (DETAILS "player1-score1-details.html"))
;;       ((DATE "20/10/1991") (SCORE "2-1")
;;        (DETAILS "player1-score2-details.html")))))
;;    ((NAME "Alejandro Narancio") (AGE "29")
;;     (MATCHES
;;      (:COLLECTION
;;       ((DATE "20/10/1995") (SCORE "3-0")
;;        (DETAILS "player2-score1-details.html"))
;;       ((DATE "20/10/1991") (SCORE "4-1")
;;        (DETAILS "player2-score2-details.html"))))))))
