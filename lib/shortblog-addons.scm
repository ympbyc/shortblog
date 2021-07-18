(import-js-symbols localStorage Date)

(let* ((e (wait-for* 1))
       (head (qsel "head"))
       (body (qsel "body")))

  (element-insert!
   head
   (element-new `(style "
textarea {width: 90%; height: 1em;}
button {border-radius: 10%; background: #CC6D0D; color: #fff}
h2>a {text-decoration: none; color:#7ea0ce}
h2>a:hover {text-decoration:underline;}
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
     (element-new `(div (textarea.post-text)))
     (qsel "article"))

  (define (array-str-pure str)
    (if (or (js-null? str)
	    (< (string-length str) 1)) "[]" str))

  (define (ls-push item x)
    (let1 arr (: JSON 'parse
		 (array-str-pure (: localStorage 'getItem item)))
	  (: arr 'push x)
	  (: localStorage 'setItem item (: JSON 'stringify arr))))

  (define (date-str-now)
    (let1 d (js-new Date)
	  (format "~a-~a-~a ~a:~a" (date-year d) (date-month d) (date-day d) (date-hour d) (date-minute d))))

  (let1 e (listen-document-event "keyup" "post-text")
	(when (= 13 (.. e 'keyCode))
	      (ls-push "shortblog_posts"
		       (list->js-array (list (.. e 'target 'value) (date-str-now))))
	      (alert (map vec->lis (vec->lis (parse-json (: localStorage 'getItem "shortblog_posts")))))
	      (js-set! (.. e 'target) "value" "")))

  )
