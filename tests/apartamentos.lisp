;; We need  this because the main URL contain spaces 
(setq *strict-parse* nil)

(def-web-extractor detalle
    ((direccion :finder (regexp-finder "<td.*?DIRECCION.*?<th.*?>(.*?)</th>"))
     (distancia-al-mar :finder (regexp-finder "<td.*?DISTANCIA MAR.*?<th.*?>(.*?)</th>"))
     (vista-al-mar :finder (regexp-finder "<td.*?VISTA AL MAR.*?<th.*?>(.*?)</th>"))
     (metraje :finder (regexp-finder "<td.*?SUPERFICIE EDIFICADA.*?<font.*?>(.*?)</font>"))
     (piso :finder (regexp-finder "<td.*?PISO.*?<th.*?>(.*?)</th>"))
     (ubicacion :finder (regexp-finder "<td.*?UBICACION.*?<th.*?>(.*?)</th>"))
     (ano :finder (regexp-finder "<td.*?DE CONSTRUCCION APROX.*?<th.*?>(.*?)</th>"))
     (estado :finder (regexp-finder "<td.*?ESTADO.*?<th.*?>(.*?)</th>"))
     (nro-dormitorios :finder (regexp-finder "<td.*?DORMITORIOS.*?<th.*?>(.*?)</th>"))))



(def-web-extractor apto
    ((detalle-apto :follow detalle :finder (compose
					    (regexp-finder "lehio\\('(.*?)'")
					    (lambda (ref-id)
					      (concatenate 'string 
							   "http://apartamentos.com.uy/bc/inmueble.asp?idi=&pag=apartamento&juntas=s&id=G10&op=A&ref="
							   ref-id))))))
(def-web-extractor aptos-col
    ((aptos :collection apto
	    :splitter (xpath-splitter "//tbody/tr[@bgcolor]")
	    :limit 10
	    :next-page-gen (param-pager "&orria=()" :init 2 :inc 1))))

(defparameter aptos-c nil)

(time (setq aptos-c (extract :url "http://apartamentos.com.uy/bc/apartamentosalquiler.asp?where=WHERE alquileres.alquilado is not null&indinmobiliaria=000&idgrupo=G10&nprop=99"
			     :struct-map aptos-col)))
                                   

;; URL detalles apto
;; http://apartamentos.com.uy/bc/inmueble.asp?idi=&pag=apartamento&juntas=s&id=G10&ref=ACMAPa003&op=A

