;; http://www.atpworldtour.com/Scores/Archive-Event-Calendar.aspx

(def-web-extractor tournament
    ((date :finder (xpath-finder "/tr/td[1]"))
     (name :finder (xpath-finder "/tr/td[2]"))
     (surface :finder (xpath-finder "/tr/td[3]"))
     (prize :finder (xpath-finder "/tr/td[4]"))
     (winner :finder (xpath-finder "/tr/td[6]/a[1]"))))

(def-web-extractor tournaments-col
    ((tournaments :collection tournament
		:limit 120
		:splitter (xpath-splitter "//div[@class='calendarTable']/table/tbody/tr")
		:next-page-gen (param-pager "?t=2&y=()" :init 2011 :inc 1))))

(setq atp-tournaments (extract :url "http://www.atpworldtour.com/Scores/Archive-Event-Calendar.aspx" :struct-map tournaments-col))
