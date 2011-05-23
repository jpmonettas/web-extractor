(defsystem cl-web-extractor
  :name "Web-Extractor"
  :description "Framework for extract data from web pages."
  :version "0.01"
  :author "Juan Monetta <jpmonettas@gmail.com>"
  :licence "MIT License"
  :depends-on (:drakma :cl-ppcre :cl-libxml2)
  :components ((:file "packages")
               (:file "web-extractor") ))





