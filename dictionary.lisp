;; dictionary generator
;; generates dictionally
;; Minori Yamashita Dec. 2021

(in-package :miniblog)

(defparameter *dictionary-home* "~/Work/miniblog/dictinary")

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


(defun make-dict-entry (title description)
  (with-dict-out out "text.txt"
    (format out "~a ... ~a @~a ~a~%" title description (today-str) (time-str))))

(defun dict-search (text)
  (with-dict-in in "text.txt"
    (loop for line = (read-line ,in nil nil)
       while line
       when (ppcre:scan text line)
       do (format nil "~a~%" line))))

(defun attach-media (title media-path)
  (with-dict-out out "media.txt"
    (format out "~a ... ~a" title media-path (today-str) (time-str))))
