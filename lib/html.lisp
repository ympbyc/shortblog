;; Copyright (c) 2020 Mark Polyakov
;; Released under the WTFPL (Do What The Fuck You Want To Public License)

(defpackage :html
  (:use :cl)
  (:export :html->string))

(in-package :html)

(defvar *html-void-tags* '(:area :base :br :col :embed :hr :img :input :link
                              :meta :param :source :track :wbr)
  "String designators for self-closing/void tags.
https://html.spec.whatwg.org/multipage/syntax.html#void-elements")

(defvar *html-escapes*
  '(#\> "&gt;"
    #\< "&lt;"
    #\& "&amp;"
    #\" "&quot;"))

(defun escape (str)
  (declare (string str))
  (with-output-to-string (stream)
    (loop for ch across str
       for escaped = (getf *html-escapes* ch)
       do (if escaped
              (write-string escaped stream)
              (write-char ch stream)))))

(defun all-upper-case-p (str)
  (reduce #'(lambda (x y) (and x (upper-case-p y))) str :initial-value T))

(defun tag-string (sym)
  (let ((tag (symbol-name sym)))
    (if (all-upper-case-p tag)
	(string-downcase sym)
	tag)))

(defun html->string (html)
  "The argument should be of the form (tag-name (attr-name attr-val) child1
child2 ...). Attributes and children are optional.

    (html->string
       '(html ()
            (head ()
                (title () \"My awesome website!\"))
            (body ()
                \"Hello! I'm Mark.\"
                ;; No attributes or children:
                (br)
                (a (href \"https://github.com/markasoftware\") \"My stuff\")
                (br)
                ;; No children:
                (img (src \"/cats.jpg\" alt \"My cute cats!\")))))

Since the argument must be quoted, you can use backquote notation to interleave
html and lisp:

    `(div ()
         \"My name is \" 
         ,*my-name*
         \", But you can call me:\"
         (br)
         (ul ()
             ,@(mapcar (lambda (name) `(li () ,name)) *my-nicknames*)))

All text and attribute values are escaped properly. You can use keyword symbols for tag and
attribute names if you'd like. There's a hardcoded list of self-closing tags, such as br and img.

Unescaped HTML can be inserted as a string using (:noescape \"<div>\"), for example. This is the
only time the second argument may be a string, so :noescape can still be used to designate
<noescape>."
  (etypecase html
    (null "")
    (string (escape html))
    (number (write-to-string html))
    (cons
     (cond
       ;; if html is not a valid html element, assume it's a list of html elements and recurse
       ((or (null (car html))
            (not (symbolp (car html))))
        (apply #'concatenate 'string (mapcar #'html->string html)))
       ((and (eq :noescape (car html))
             (stringp (cadr html)))
        (cadr html))
       (t (destructuring-bind
                ;; the &key business forces an even number of arguments.
                (tag &optional ((&rest attrs &key &allow-other-keys)) &rest body)
              html
            (check-type tag symbol)
            ;; printf is a child's toy. Honestly, regex might be too!
            (format nil "<~A~:{ ~A=\"~A\"~}~:[/>~;>~A</~A>~]"
                    (tag-string tag)
                    (loop for (attr-name attr-val) on attrs by #'cddr
                       collect
                         (list (string-downcase attr-name)
                               (escape (etypecase attr-val
                                         ((or string symbol) (string attr-val))
                                         (number (write-to-string attr-val))))))
                    (not (member tag *html-void-tags* :test #'string=))
                    (apply #'concatenate 'string (mapcar #'html->string body))
                    (tag-string tag))))))))
