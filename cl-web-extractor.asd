(defpackage #:cl-web-extractor-asd
  (:use :cl :asdf))

(in-package :cl-web-extractor-asd)


(defsystem cl-web-extractor
  :name "Web-Extractor"
  :description "Framework for extract data from web pages."
  :version "0.01"
  :author "Juan Monetta <jpmonettas@gmail.com>"
  :licence "MIT License"
  :depends-on (:drakma :puri :cl-ppcre :cl-html-parse)
  :components ((:file "packages")
               (:file "web-extractor")))





