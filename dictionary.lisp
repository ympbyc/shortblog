;; dictionary generator
;; generates dictionary
;; Minori Yamashita Dec. 2021

(in-package :miniblog)

(defparameter *dictionary-home* "~/Work/miniblog/dictionary")

(defmacro with-dict-out (out file &body body)
  `(progn
     (ensure-directories-exist ,*dictionary-home*)
     (with-open-file (,out (format nil "~a/~a" ,*dictionary-home* ,file)
			   :direction :output
			   :if-exists :append
			   :if-does-not-exist :create)
       ,@body)))

(defmacro with-dict-in (in file &body body)
  `(progn
     (ensure-directories-exist ,*dictionary-home*)
     (with-open-file (,in (format nil "~a/~a" ,*dictionary-home* ,file)
			  :direction :input)
       ,@body)))


(defun dict-make-entry (title description)
  (with-dict-out out "dict.txt"
    (format out "~a ... ~a @~a~%" title description (today-str))))

(defun dict-search (regex)
  (with-dict-in in "dict.txt"
    (loop for line = (read-line in nil nil)
       while line
       when (ppcre:scan regex line)
       do (format t "~a~%" line))))

(defun dict-attach-media (title source-path)
  (with-dict-out out "media.txt"
    (let ((filename (media-copy-name source-path)))
      (copy-file source-path
		 (format nil "~a/~a" *dictionary-home* filename))
      (format out "~a ... ~a @~a~%" title filename (today-str)))))

