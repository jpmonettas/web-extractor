(def-web-extractor cancha-map 
    ((nombre :finder (xpath-finder "//*[@id=\"content\"]/table/tbody/tr[6]/td[2]"))
     (pais :finder (xpath-finder "//*[@id=\"content\"]/table/tbody/tr[7]/td[2]"))
     (departamento :finder (xpath-finder "//*[@id=\"content\"]/table/tbody/tr[8]/td[2]"))
     (ciudad :finder (xpath-finder "//*[@id=\"content\"]/table/tbody/tr[9]/td[2]"))
     (barrio :finder (xpath-finder "//*[@id=\"content\"]/table/tbody/tr[10]/td[2]"))
     (telefonos :finder (xpath-finder "//*[@id=\"content\"]/table/tbody/tr[11]/td[2]"))
     (direccion :finder (xpath-finder "//*[@id=\"content\"]/table/tbody/tr[12]/td[2]"))
     (web :finder (xpath-finder "//*[@id=\"content\"]/table/tbody/tr[13]/td[2]"))
     (email :finder (xpath-finder "//*[@id=\"content\"]/table/tbody/tr[14]/td[2]"))
     (cerradas :finder (compose 
			(xpath-finder "//*[@id=\"content\"]/table/tbody/tr[15]/td[2]")
			(regexp-finder ".*(Si).*"))
			:try-cast-type BOOLEAN)
     (observaciones :finder (xpath-finder "//*[@id=\"content\"]/table/tbody/tr[16]/td[2]"))
     (lat :finder (compose 
		   (xpath-finder "//*[@id=\"content\"]/table/tbody/tr[18]/td/iframe/@src")
		   (regexp-finder "&ll=(.*?),")))
     (long :finder (compose 
		    (xpath-finder "//*[@id=\"content\"]/table/tbody/tr[18]/td/iframe/@src")
		    (regexp-finder "&ll=.*?,(.*?)&")))))


(def-web-extractor cancha-follow-map
    ((cancha-detalle :follow cancha-map 
		    :finder (compose 
			     (xpath-finder "//a/@href")
			     (lambda (h) 
			       (concatenate 'string "http://www.futbol5.com.uy/" h))))))

(def-web-extractor canchas-map
    ((canchas-col :collection cancha-follow-map
	      :splitter (xpath-splitter "//tbody/tr[position()>1]")
	      :limit 500)
     (pais :finder (lambda (h) "URUGUAY"))))

(setq canchas (extract :url "http://www.futbol5.com.uy/ajax/results.php?estado=10&pais=1"
		       :struct-map canchas-map))


(collect-items canchas 
	       (with-direct-attribute-value 
		   'DEPARTAMENTO 
		 (lambda (v) 
		   (not (equal v "Montevideo")))))

















