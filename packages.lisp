(defpackage :web-extractor
  (:use :cl :drakma :cl-ppcre :xtree :iter :puri)
  (:documentation "Main package for web-extractor.")
  (:nicknames :webe)
  (:export
   #:regexp-finder
   #:xpath-splitter
   #:def-web-extractor
   #:extract))


