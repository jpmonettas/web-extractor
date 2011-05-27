;; This is for parsing http://twitaholic.com/

(def-web-extractor twitaholic-map
    ((number :finder (xpath-finder "/tr/td[1]"))
     (name :finder (xpath-finder "/tr/td[3]/a"))
     (followers :finder (xpath-finder "/tr/td[6]"))))

(def-web-extractor twitaholics-map
    ((twitaholics :collection twitaholic-map
		  :splitter (xpath-splitter "/html/body/div/div[4]/div[2]/table/tbody/node()"))))

(extract :url "http://twitaholic.com/" :struct-map twitaholics-map)



