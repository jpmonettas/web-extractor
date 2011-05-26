;; This is for parsing http://twitaholic.com/


(defparameter *test-data* (webe::file-string "/home/juan/MyProjects/web-extractor/tests/real-test1-2.html"))

(def-web-extractor twitaholic-map
    ((number :finder )
     (name :finder)
     (followers :finder)))

(def-web-extractor twitaholics-map
    ((twitaholics :collection nil :splitter (xpath-splitter "/html/body/div/div[4]/div[2]/table/tbody/node()"))))

(extract :url "http://twitaholic.com/" :struct-map twitaholics-map)

(extract :str *test-data* :struct-map twitaholics-map)

