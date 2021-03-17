;; super tiny blog engine

(defpackage :miniblog
  (:use #:CL #:uiop #:ppcre)
  (:export :make-post :add-media :show-posts :generate-thumbnails :save-html))

(load "lib/html.lisp")

(in-package :miniblog)

(defparameter *blog-home* "~/Work/miniblog")
(defparameter *blog-title*  "NORISOFT diary")
(defparameter *thumbnail-size* "320")
(defparameter *blur-faces* t)

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
			       (today-str) (regex-replace "\:" (time-str) "-")
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
				     ,@(when facep `("+noise" "Gaussian" "-noise" "4"))
				,thumb-name)
			     :search t :wait t :output out))))))))

(defun html-head (title)
  `(head () (meta (charset "utf-8"))
	 (title () ,title)
	 (style () "
img{max-width:100%} 
p  {line-height:1.8em}")))


(defun build-html (dir text-txt blog-html)
  (let ((date "")
	(title (format nil "~a | ~a" (pathname-month dir) *blog-title*)))
    (with-open-file (in text-txt)
	  (with-open-file (out blog-html
			       :direction :output
			       :if-exists :supersede
			       :if-does-not-exist :create)
	    (println "<!doctype html>" out)
	    (println (html:html->string
		      `(html (lang "ja")
			     ,(html-head title)
			     (body ()
				   (h1 () ,title)
				   ,@(loop for line in (reverse (read-file-lines in))
					for day = (subseq (ppcre:scan-to-strings "\@([^\ ]+)" line) 1)
					collect `(,(if (string/= date day)
						       (progn (setq date day) `((h2 () ,day))))
						   ,(cond
						      ((string= "FILE:" (subseq line 0 5))
						       (multiple-value-bind (x match)
							   (ppcre:scan-to-strings "FILE:\ ([^\@]+)" line)
							 (declare (ignore x))
							 `(img (src ,(format nil "thumbs/~a" (aref match 0)) width 320))))
						      (t `(p ()
							     ,(coerce (scan-group "(.*)@.*\ (.*)$" line) 'list))))))
				   (a (href "../index.html") "->INDEX"))))
		     out)))))

(defun save-html ()
  (generate-thumbnails)
  (loop for dir in (blogs-across-months)
     for text-txt   = (format nil "~atext.txt" dir)
     for blog-html = (format nil "~aindex.html" dir)
     for html-exists = (uiop:file-exists-p blog-html)
     for date = ""
       ;;process new files only
     when (or (not html-exists)
	      (and html-exists
		   (< (uiop:safe-file-write-date blog-html)
		      (uiop:safe-file-write-date text-txt))))
     do (build-html dir text-txt blog-html))
  (with-open-file (out (format nil "~a/index.html" (blog-rel-path "blog"))
		       :direction :output
		       :if-exists :supersede
		       :if-does-not-exist :create)
    (println "<!doctype html>" out)
    (println (html:html->string
	      `(html (lang "ja")
		     ,(html-head (format nil "~a index" *blog-title*))
		     (body () (h1 (),*blog-title*)
			   (ul ()
			       ,(loop for dir in (blogs-across-months)
				   for blog = (format nil "~a/index.html" (pathname-month dir))
				   collect `(li () (a (href ,blog) ,(pathname-month dir))))))))
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
