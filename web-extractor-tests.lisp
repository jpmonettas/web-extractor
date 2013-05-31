(in-package :extractor)

(define-test attribute-p-test
  (assert-true (attribute-p '(test "a")))
  (assert-false (attribute-p '(test)))
  (assert-false (attribute-p 'test))
  (assert-false (attribute-p '(test (test test)))))

(define-test tree-to-property-list-test
  (assert-equal (tree-to-property-list '(A (B "B") (C (D "D"))))
		'(B "B" D "D")))

(define-test regexp-finder-test
  (assert-equal (funcall 
		 (regexp-finder ">Name \\[(.*)\\]<")
		 "<html>Name [Juan]</html>")
		"Juan"))

(define-test xpath-finder-test
  (assert-equal (funcall
		 (xpath-finder "/html/a/@href")
		  "<html><a href=\"test.com\">Name [Juan]</a></html>")
		  "test.com"))

(define-test xpath-splitter-test
  (assert-equal (funcall
		 (xpath-splitter "/table/tr")
		 "<table><tr><td>Test1</td></tr><tr><td>Test2</td></tr></table>")
		(list "<tr><td>Test1</td></tr>" "<tr><td>Test2</td></tr>")))
		
(define-test param-pager-test
  (let ((pager-func (param-pager "?a=()&b=3" :init 4 :inc 1)))
    (loop for i from 4 to 8 do
	 (assert-equal (funcall pager-func "http://test.com" nil) 
		       (format nil "http://test.com?a=~a&b=3" i)))))

(define-test get-property-value-test
  (let ((test-tree '(A ((B "B" ) (C (D "D"))))))
    (assert-equal (get-property-value "A" test-tree)
		  '((B "B") (C (D "D"))))
    (assert-equal (get-property-value "B" test-tree)
		  '("B"))
    (assert-equal (get-property-value "C" test-tree)
		  '(D "D"))
    (assert-equal (get-property-value "D" test-tree)
		  '("D"))))

(run-tests)
