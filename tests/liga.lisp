(in-package :extractor)

(def-web-extractor partido-map
    ((local :finder (xpath-finder "//div[@class='equipo'][1]"))
     (visitante :finder (xpath-finder "//div[@class='equipo'][2]"))
     (resultado_local :finder (compose 
                          (xpath-finder "//div[@class='vs']")
                          (regexp-finder "(.*)-")))
     (resultado_visitante :finder (compose 
                          (xpath-finder "//div[@class='vs']")
                          (regexp-finder "-(.*)")))))

(def-web-extractor fecha-map
    ((fecha :finder (compose
                     (xpath-finder "/root/div[@id='titulo-fecha']" :add-root t)
                     (regexp-finder "([0-9]+)")))
     (partidos :collection partido-map
               :splitter (xpath-splitter "/root/div[position()>1]" :add-root t))))

(def-web-extractor fechas-map
    ((fechas :collection fecha-map
             :splitter (regexp-splitter "(<div id=\"titulo-fecha\".*?)<div id=\"titulo-fecha\""))))

     
(def-web-extractor divisional-map
    ((nombre :finder (xpath-finder "//a"))
     (fechas-col :follow fechas-map :finder 
                 (xpath-finder "//a/@href"))))

(def-web-extractor divisionales-map
    ((divisionales :collection divisional-map
                   :splitter  (xpath-splitter "//*[@id='adtabla']/div/a")
                   :limit 10)))

(defun parse-liga ()
  (print 
   (json:encode-json 
    (extract :url "http://www.todoliga.com.uy/tablas/partidos/listar/categoria/1/accion/resultados.html" 
             :struct-map divisionales-map))))

;;(push "/home/jmonetta/MyProjects/web-extractor/" ASDF:*CENTRAL-REGISTRY*)
;;(asdf:oos 'asdf:load-op :cl-web-extractor)
;;(load "/home/jmonetta/MyProjects/web-extractor/tests/liga.lisp")
;;(sb-ext:save-lisp-and-die "my_sbcl_parse_liga" :executable t :toplevel 'extractor::parse-liga)
