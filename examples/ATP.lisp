;; This is for parse http://www.atpworldtour.com/Rankings/Singles.aspx

(def-web-extractor atp-player
    ((rank :finder (xpath-finder "/root/tr/td[1]/span" :add-root 't))
     (name :finder (xpath-finder "/root/tr/td[1]/a" :add-root 't))))

	   
(def-web-extractor atp-map
    ((players :collection atp-player
	      :limit 100
	      :splitter (xpath-splitter "//table[@class='bioTableAlt stripeMe']/tbody/tr[position()>1]")
	      :next-page-gen (param-pager "?d=23.05.2011&c=&r=()#" :init 101 :inc 100))))

    
(setq atp-players (extract :url "http://www.atpworldtour.com/Rankings/Singles.aspx" :struct-map atp-map))

(setq step1 (cdr (car (cdr (car atp-players)))))
(setq atp-step2 (loop for e in step1 collect (tree-to-property-list e)))
