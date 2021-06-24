(element-insert!
 (qsel "body")
 (element-new `(div
		 (ul ,@(map
			(lambda (todo)
			  `(li ,(.. todo 'innerHTML)))
			(append (qsel-all ".todo")
				(qsel-all ".schedule")))))))
