;; Test for parsing http://technology.timesonline.co.uk/tol/news/tech_and_web/article5641893.ece
(def-web-extractor twiters-map
    ((twiters :collection nil 
	      :splitter (compose 
			 (xpath-finder "//div[@id='related-article-links']")
			 (regexp-splitter "(<p>[\\n\\r\\t ]*<b>\\d{1}.*?)<p>[\\n\\r\\t ]*<b>\\d{1}")))))


(extract :url "http://technology.timesonline.co.uk/tol/news/tech_and_web/article5641893.ece" :struct-map twiters-map)