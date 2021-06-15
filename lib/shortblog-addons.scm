(element-insert!
 (qsel "body")
 (element-new `(div
		 (h3 "Memo")
		 (ul ,@(map
			(lambda (todo)
			  `(li ,(.. todo 'innerHTML)))
			(qsel-all ".todo"))))))
