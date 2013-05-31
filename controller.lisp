(in-package :extractor-web-app)

;;(declaim (optimize (debug 3)))

(setq *dispatch-table*
      (list
       (create-regex-dispatcher "^/$" 'main-page)
       (create-regex-dispatcher "^/eval-code$" 'eval-code)
       (create-folder-dispatcher-and-handler "/static/" "/home/jmonetta/MyProjects/web-extractor/static/")))

(setq *prologue* "<!DOCTYPE html>")

(defun eval-code ()
  (let ((code-string (parameter "code")))
    (extractor::eval-code-in-extractor code-string)))



(defun main-page ()
  (with-html-output (*standard-output* nil :prologue t)
    (:html
     (:head
      (:title "Web Extractor")
      (:meta :name "viewport" :content "width=device-width, initial-scale=1.0")
      (:link :rel "stylesheet" :href "static/bootstrap/css/bootstrap.min.css" :media "screen")
      (:link :rel "stylesheet" :href "static/site.css"))
     (:body
      (:div :class "container-fluid"
	    (:div :class "row-fluid"
		  (:div :class "span7"
			(:div
			 (:button :id "btn-evaluate" :type "button" :class "btn btn-primary" "Go"))
			(:textarea :id "code-textarea" :rows "30" :cols "200"))
		  (:div :class "span5"
			(:ul :class "nav nav-tabs" :id "right-side-tabs"
			     (:li :class "active"
				  (:a :href "#results" :data-toggle "tab" "Results"))
			     (:li 
				  (:a :href "#cache" :data-toggle "tab" "Cache"))
			     (:li 
				  (:a :href "#help" :data-toggle "tab" "Help")))
			(:div :class "tab-content"
			      (:div :class "tab-pane active" :id "results"
				    (:textarea :id "results-textarea" :rows "30" :cols "80"))
			      (:div :class "tab-pane" :id "cache")
			      (:div :class "tab-pane" :id "help")))))
      (:script :src "http://code.jquery.com/jquery.js")
      (:script :src "static/site.js")
      (:script :src "static/bootstrap/js/bootstrap.min.js")))))











