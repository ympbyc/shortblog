;; super tiny blog engine

(defpackage :miniblog
  (:use #:CL #:uiop #:ppcre)
  (:export :make-post :add-media :show-posts :generate-thumbnails :save-html))

(load "lib/html.lisp")

(in-package :miniblog)

(defparameter *blog-home* "~/Work/miniblog")
(defparameter *blog-title*  "NORI-FIX diary")
(defparameter *blog-description* "(nori-fix|ympbyc) I like to fix/remake and use old things.")
(defparameter *thumbnail-size* "640")
(defparameter *blur-faces* '("+noise" "Gaussian" "-noise" "6"))
(defparameter *public-root-url* "https://ympbyc.github.io/shortblog/blog/")
(defparameter *author-name* "norimixer")
(defparameter *language* "ja")

(defun blog-rel-path (x)
  (format nil "~a/~a" *blog-home* x))

(defmacro with-today (&body body)
  `(multiple-value-bind (s min h d mon y)
       (decode-universal-time (get-universal-time))
     ,@body))

(encode-universal-time 0 0 0 25 2 2021)

(defun month-str ()
  (with-today
    (declare (ignore s min h d))
    (format nil "~4,'0d-~2,'0d" y mon)))

(defun today-str ()
  (with-today
    (declare (ignore s min h))
    (format nil "~4,'0d-~2,'0d-~2,'0d" y mon d)))

(defun time-str ()
  (with-today (declare (ignore d mon y s))
	      (format nil "~2,'0d:~2,'0d" h min)))


(defmacro with-blogdir (&body body)
  `(let ((blogdir (format nil "~a/~a/" (blog-rel-path "blog/") (month-str))))
     (ensure-directories-exist blogdir)
     ,@body))

(defun make-post (body)
  (with-blogdir
    (with-open-file (out (format nil "~a~a" blogdir "text.txt")
			 :direction :output
			 :if-exists :append
			 :if-does-not-exist :create)
      (format out "~a @~a ~a~%" body (today-str) (time-str)))))

(defun add-media (source-path)
  (with-blogdir
    (let ((source-path (uiop:ensure-pathname source-path))
	  (filename    (format nil "~a-~a-~a.~a"
			       (pathname-name source-path)
			       (today-str) (regex-replace "[\:\ ]" (time-str) "-")
			       (pathname-type source-path))))
      (copy-file
       source-path (format nil "~a~a" blogdir filename))
      (make-post (format nil "FILE: ~a" filename)))))

(defun pathname-month (d) (car (last (pathname-directory d))))

(defun show-posts (&optional month)
  (declare (ignorable month))
  (loop for dir in (sort (subdirectories (blog-rel-path "blog/")) #'string>
			 :key #'pathname-month)
     do (with-open-file (stream (format nil "~a/text.txt" dir))
	  (println (pathname-month dir))
	  (loop for line in (reverse (read-file-lines stream))
	     do (println line)))))


(defmacro scan-group (regex str)
  `(cadr (multiple-value-list (ppcre:scan-to-strings ,regex ,str))))

(defmacro blogs-across-months ()
  `(sort (subdirectories (blog-rel-path "blog/")) #'string>
	 :key #'pathname-month))

(defun generate-thumbnails (&key force)
  (loop for dir in (blogs-across-months)
     do (loop for file in (uiop:directory-files dir)
	     when (member (string-upcase (pathname-type file)) '("JPG" "PNG" "GIF") :test #'string=)
	   do (let* ((thumbs-dir (format nil "~athumbs/" dir))
		     (thumb-name (format nil "~a~a.~a" thumbs-dir (pathname-name file) (pathname-type file)))
		     (facep nil))
		(ensure-directories-exist thumbs-dir)
		(when (or force (not (uiop:file-exists-p thumb-name)))
		  (if *blur-faces*
		      (handler-case
			  (setq facep
				(= 0 (sb-ext:process-exit-code
				      (sb-ext:run-program "facedetect" `("-q" ,(format nil "~a" file)) :search t :wait t))))
			(t (c) (format t "NOTICE: ~a" c))))
		  (format t "~a: ~a~%" file
			  (with-output-to-string (out)
			    (sb-ext:run-program
			     "convert"
			     `(,(format nil "~a" file) "-thumbnail" ,*thumbnail-size*
				     ,@(when facep *blur-faces*)
				,thumb-name)
			     :search t :wait t :output out))))))))

(defun html-head (title &optional indexp)
  `(head () (meta (charset "utf-8"))
	 (title () ,title)
	 (meta (name "description" content ,*blog-description*))
	 (meta (name "viewport" content "width=device-width,initial-scale=1"))
	 ,(unless indexp `(script (src "../../lib/biwascheme-0.7.2.js")))
	 ,(unless indexp `(script (type "text/biwascheme") (:noescape "(load \"../../lib/pieces-biwa.scm\") (load \"../../lib/shortblog-addons.scm\")")))
	 (style () (:noescape "
body{background: rgb(236,235,230); font-family:\"Droid Sans Fallback\"}
h1 {font-size: 1.6em; padding-right:1em; background:rgba(230,194,19,0.3); text-align:right}
h2 {font-size: 1.2em; }
img{max-width:100%} 
p  {line-height:1.8em; margin: 1em 0}
* {margin: 0; padding: 0}
article {border-bottom: 2px solid rgb(230,194,19); margin: 2em 0;padding: 0 1em}
article p, .profile{background:rgba(255,255,255,0.2); padding: 0 1em}
article .time{color:#c3c3c3;font-size:0.7em; float:right}
ul {list-style-type:circle}
.link-top {float:right;padding-right:1.6em}
.icon {border-radius:50%; width: 60px;height:60px;}
.profile {margin: 1em 0; padding: 1em; width:max-content; overflow:hidden; max-width:100%;box-sizing:border-box}
ul.blog-list {margin: 1em 2em}
.body-compact {width: max-content; max-width:100%; box-sizing:border-box; margin: 0 auto}"))))


(defmacro match-0 (s-t-s)
  `(multiple-value-bind (_ match)
       ,s-t-s
     (declare (ignore _))
     (aref match 0)))

(defun post-file-path (post)
  (match-0 (ppcre:scan-to-strings "FILE:\ ([^\@]+)" post)))

(defun make-style (text)
  (cond ((and (> (length text) 11) (string= "HIGHLIGHT:" (subseq text 0 10)))
	 `(b () ,(subseq text 10)))
	((and (> (length text) 6) (string= "TODO:" (subseq text 0 6)))
	 `(span (class todo) ,text))
	(t text)))


(defmacro collect-articles (in-txt make-article)
  `(let ((date "")
	 (posts/day ())
	 (xs ()))
     (loop for line = (read-line ,in-txt nil nil)
	while line
	for day = (subseq (ppcre:scan-to-strings "\@([^\ ]+)" line) 1)
	do (if (string/= day date)
	       (progn
		 (if posts/day (push ,make-article xs))
		 (setq date day)
		 (setq posts/day (list line)))
	       (push line posts/day)))
     (push ,make-article xs)
     xs))

(defun private? (post)
  (string= "PRIVATE:" (subseq post 0 8)))

(defun build-articles (in-txt)
  (collect-articles in-txt
		    `(article ()
			      (h2 () ,date)
			      ,@(loop for post in posts/day
				   collect (cond
					     ((string= "FILE:" (subseq post 0 5))
					      `(img (src ,(format nil "thumbs/~a" (post-file-path post)) width 320
							 alt ,date)))
					     ((private? post)
					      `())
					     (t `(p ()
						    ,(let ((x (scan-group "(.*)@.*\ (.*)$" post)))
						       `(,(make-style (aref x 0)) (span (class "time") ,(aref x 1)))))))))))




(defun build-html (dir text-txt blog-html)
  (println "building html....")
  (let ((title (format nil "~a | ~a" (pathname-month dir) *blog-title*)))
    (with-open-file (in text-txt)
	  (with-open-file (out blog-html
			       :direction :output
			       :if-exists :supersede
			       :if-does-not-exist :create)
	    (println "<!doctype html>" out)
	    (println (html:html->string
		      `(html (lang ,*language*)
			     ,(html-head title)
			     (body ()
				   (h1 () ,title)
				   (a (href "../index.html" class "link-top") "->INDEX")
				   ,@(build-articles in)
				   (a (href "../index.html") "->INDEX"))))
		     out)))))

(defun build-index ()
  (with-open-file (out (format nil "~a/index.html" (blog-rel-path "blog"))
		       :direction :output
		       :if-exists :supersede
		       :if-does-not-exist :create)
    (println "<!doctype html>" out)
    (println (html:html->string
	      `(html (lang "ja")
		     ,(html-head (format nil "~a index" *blog-title*) t)
		     (body (class "body-compact") (h1 (),*blog-title*)
			   (section (class "profile")
			      (h2 (style "float:right;margin:0 1em 0 0") "Author")
			      (img (class "icon"
				    src "profile.jpg"
				    alt "profile picture of the author"))
			      (br ()) ,*blog-description*)
			   (ul (class "blog-list")
			       ,(loop for dir in (blogs-across-months)
				   for blog = (format nil "~a/index.html" (pathname-month dir))
				   collect `(li () (a (href ,blog) ,(pathname-month dir))))))))
	     out)))

(defun save-html (&key force)
  (generate-thumbnails)
  (loop for dir in (blogs-across-months)
     for text-txt   = (format nil "~atext.txt" dir)
     for blog-html = (format nil "~aindex.html" dir)
     for html-exists = (uiop:file-exists-p blog-html)
     for date = ""
       ;;process new files only
     when (or force
	      (not html-exists)
	      (and html-exists
		   (< (uiop:safe-file-write-date blog-html)
		      (uiop:safe-file-write-date text-txt))))
     do (build-html dir text-txt blog-html))
  (build-index))

(defparameter days '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))
(defparameter months '("Dec" "Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))

(defun rfc822-date (univ-time)
  (multiple-value-bind (sec min hour date month year dow dls timezone)
      (decode-universal-time univ-time) (declare (ignore dls)) (format nil "~a, ~2,'0d ~a ~4,'0d ~2,'0d:~2,'0d:~2,'0d ~a~2,'0d00" (nth dow days) date (nth month months) year hour min sec (if (< (* -1 timezone) 0) "-" "+") (* -1 timezone))))

(defun post-date-str (post)
  (match-0
      (scan-to-strings "@([\\d\:\-\\s]+)" post)))

(defun post-date (post)
  (let* ((dt (reverse (mapcar #'parse-integer (split-string (post-date-str post)
							    :separator "\ |\:|\-"))))
	 (dt (if (< (length dt) 6) (cons 0 dt) dt)))
    (setf (nth 2 dt) (mod (nth 2 dt) 24))
    (apply #'encode-universal-time dt)))

(defun save-rss ()
  (with-open-file (out (blog-rel-path "blog/feed.rss")
		       :direction :output
		       :if-exists :supersede
		       :if-does-not-exist :create)
    (println "<?xml version=\"1.0\" encoding=\"utf-8\"?>" out)
    (println (html:html->string
	      `(rss (version "2.0")
		    (channel ()
			     (title () ,*blog-title*)
			     (lang () ,*language*)
			     (|link| () ,(format nil "~aindex.html" *public-root-url*))
			     (copyright () ,(format nil "Copyright (c) ~a" *author-name*))
			     (|lastBuildDate| () ,(rfc822-date (get-universal-time)))
			     (generator () "shortblog https://github.com/ympbyc/shortblog")
			     ,@(loop for dir in (blogs-across-months)
				  for text-txt   = (format nil "~atext.txt" dir)
				  collect (with-open-file (in text-txt)
					    (collect-articles
					     in `(item ()
						       (title () ,date)
						       (description () ,(format nil "~a"
										(remove-if #'private? posts/day)))
						       (|pubDate| () ,(rfc822-date (post-date (car posts/day)))))))))))
	     out)))


(defun function-named (name)
  (symbol-function (find-symbol (string-upcase name) 'miniblog)))

(defun help ()
  (println "USAGE: shortblog COMMAND args...")
  (println "COMMANDS: ")
  (let ((s-out (make-string-output-stream)))
      (do-external-symbols (x 'miniblog)
	(describe x s-out)
	(let ((desc (make-string-input-stream
		     (get-output-stream-string s-out))))
	  (read-line desc)(read-line desc)
	  (read-line desc) ;; drop 3 lines
	  (println (ppcre:scan-to-strings "[^\ ]+" (read-line desc)))
	  (println (read-line desc))))))

;;CLI interface
(defun main (argv)
  (format t ";; ~s~%" argv)
  (handler-case
      (progn (apply (function-named (cadr argv))
		    (cddr argv))
	     (println "OK.")
	     0)
    (t (c)
      (format t "Exception: ~a~%" c)
      (help)
      (values 1 c))))
