(defpackage :extractor
  (:use :cl :drakma :cl-ppcre :xtree :iter :puri :lisp-unit)
  (:documentation "Main package for the extractor.")
  (:nicknames :extractor)
  (:export
   #:regexp-finder
   #:xpath-splitter
   #:def-web-extractor
   #:extract))


(defpackage :extractor-web-app
  (:use :cl :cl-who :hunchentoot :json)
  (:documentation "Main package for the extractor web application")
  (:nicknames :web-app))



