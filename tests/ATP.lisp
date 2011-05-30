;; This is for parse http://www.atpworldtour.com/Rankings/Singles.aspx

(def-web-extractor atp-player
    ((rank :finder (xpath-finder "/root/tr/td[1]" :add-root 't))
     (name :finder (xpath-finder "/root/tr/td[2]/a" :add-root 't))))

(def-web-extractor atp-map
    ((players :collection atp-player
	      :limit 230
	      :splitter (xpath-splitter "//table[@class='bioTableAlt stripeMe']/tbody/tr[position()>1]")
	      :next-page-gen (param-pager "?d=23.05.2011&c=&r={}#" :init 101 :inc 100))))

    
(extract :url "http://www.atpworldtour.com/Rankings/Singles.aspx" :struct-map atp-map)