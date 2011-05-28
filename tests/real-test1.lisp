;; This is for parsing http://twitaholic.com/

(def-web-extractor twitaholic-details-map
    ((join-date :finder (compose (xpath-finder "//div[@id='user_details']/ul/li[1]") (regexp-finder "(\\d{4}-\\d{2}-\\d{2})")))
     (website :finder (xpath-finder "//div[@id='user_info']/a[2]/@href"))))

(def-web-extractor twitaholic-map
    ((number :finder (xpath-finder "/tr/td[1]"))
     (name :finder (xpath-finder "/tr/td[3]/a"))
     (followers :finder (xpath-finder "/tr/td[6]"))
     (details :follow twitaholic-details-map :finder (xpath-finder "/tr/td[3]/a/@href")))) 
     
(def-web-extractor twitaholics-map
    ((twitaholics :collection twitaholic-map
		  :splitter (xpath-splitter "/html/body/div/div[4]/div[2]/table/tbody/tr[position()>1]")
		  :limit 10)))

(extract :url "http://twitaholic.com/" :struct-map twitaholics-map)

(extract :url "http://twitaholic.com/ladygaga/" :struct-map twitaholic-details-map)



