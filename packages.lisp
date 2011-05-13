(defpackage #:web-crawler
  (:use :cl :drakma :puri :cl-ppcre :html-parse :unique-queue)
  (:documentation "Main package for web-extractor.")
  (:nicknames :webe)
  (:export
   #:extract))


