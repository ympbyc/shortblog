(element-insert!
 (qsel "body")
 (element-new `(div
		 (ul ,@(map
			(lambda (todo)
			  `(li ,(.. todo 'innerHTML)))
			(qsel-all ".todo"))))))