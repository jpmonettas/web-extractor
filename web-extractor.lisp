(in-package :web-extractor)

(defun extract () 1)

;; (pprint
;;  (parse-html 
;;   (http-request "http://www.google.com")))

(setq *atp-ranking* (drakma:http-request "http://www.atpworldtour.com/Rankings/Singles.aspx" :user-agent :explorer))

(define-sanitize-mode links
    :elements (:a) 
    :attributes ((:a :href)))

(setq *atp-ranking-links* (parse-html (clean *atp-ranking* links)))