(import-js-symbols localStorage)

(let* ((e (wait-for* 2))
       (head (qsel "head"))
       (body (qsel "body")))

  (element-insert!
   head
   (element-new `(style "
textarea {width: 90%; height: 2em;}
button {border-radius: 10%; background: #CC6D0D; color: #fff}
")))

  (element-insert!
   body
   (element-new `(div
		  (ul ,@(map
			 (lambda (todo)
			   `(li ,(.. todo 'innerHTML)))
			 (append (qsel-all ".todo")
				 (qsel-all ".schedule")))))))

  (: body 'insertBefore
     (element-new `(div (textarea#post-text) (button.btn-post "make-post")))
     (qsel "article"))

  (defun (array-str-pure str)
    (if (or (js-null? str)
	    (< (length str) 1)) "[]" str))

  (defun (ls-push item x)
    (let1 arr (: JSON 'parse
		 (array-str-pure (: localStorage 'getItem item)))
	  (: arr push x)
	  (: localStorage 'setItem item (: JSON stringify arr))))

  (let1 e (listen-document-event "click" "btn-post")
	(ls-push "shortblog_posts"
		 (.. (qsel "#post-text") 'value))
	(alert (: localStorage 'getItem "shortblog_posts"))))
