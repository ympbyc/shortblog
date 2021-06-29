;;;;;;;;;;;;;;;;;;;;;
;;; generic utils ;;;
;;;;;;;;;;;;;;;;;;;;;
(print "loading generic utils")

;;convenient shorthand for lambda (^ (x) x)
(define-macro (^ params . expr)
  `(lambda ,params ,@expr))

;;normalize parameter list e.g. (x), xs, (x . xs) into (x), (xs), (x xs)
(define (flatten-param-list xs)
  (cond ((null? xs) ())
	((pair? xs)
	 (cons (car xs)
	       (flatten-param-list (cdr xs))))
	(else (cons xs ()))))

;;when-let1
(define-macro (when-let1 sym expr . body)
  `(let ((,sym ,expr))
     (when ,sym ,@body)))
(define-macro (if-let1* sym expr tbranch ebranch)
  `(let ((,sym ,expr))
     (if (not (falsy? ,sym)) ,tbranch ,ebranch)))

;;set! that returns the value
(define-macro (set!! sym val)
  `(begin (set! ,sym ,val)
	  ,val))

(define-macro (push! x xs)
  `(set! ,xs (cons ,x ,xs)))

(define-macro (push!! x xs)
  `(set!! ,xs (cons ,x ,xs)))

;;cdr o assoc
(define-macro (ascdr sym alist)
  `(cdr (assoc ,sym ,alist)))

;;wrap the value in a list if it's not already a list
(define-macro (list-pure x)
  (if (pair? x) x
      `(list ,x)))

(define-macro (let*-pat- binds body)
  (if (null? binds) body
      `(apply
	(lambda ,(caar binds)
	  (let-pat- ,(cdr binds) ,body))
	(list-pure ,(cadar binds)))))

;; WARNING: heavy
;; kind of like destructuring-bind
(define-macro (let*-pat binds . body)
  `(let-pat- ,binds ,(cons 'begin body)))


;;;;;;;;;;;;;;;;;;;;
;;; js-interface ;;;
;;;;;;;;;;;;;;;;;;;;
(print "loading js interface")

;; <o> eye: lousy ref
(define(<o> field obj)
  (let1 res (cond ((pair? obj)
                   (cdr (assoc (symbol->string field) obj)))
                  ((falsy? obj) ())
                  ((vector? obj)
                   (if (<= (vector-length obj) field) ()
                       (vector-ref obj field)))
                  (else (js-ref obj field)))
        (if (falsy? res) () res)))

(define-macro (<?> k)
  `(<o> ,k ?))

;; shorthand for js-invoke
(define-macro (: obj method . args)
  `(js-invoke ,obj ,method ,@args))

;; load js symbols into toplevel
(define-macro (import-js-symbols . syms)
  `(begin
    ,@(map
       (lambda (sym)
	 `(define ,sym (js-eval ,(symbol->string sym))))
       syms)))

(import-js-symbols window document console BiwaScheme Object
		   Function JSON Array encodeURI)

;;destructure js obj (shallow)
(define-macro (let-js-obj-fields binds obj . body)
  `(let ,(map (^ (bind) `(,bind (js-ref ,obj ,(symbol->string bind)))) binds)
     ,@body))

;;js-closure with proper parameter handling
(define-macro (js-close fn)
  (let ((sym1 (gensym)))
    `(js-closure (lambda (,sym1)
                   (if (vector? ,sym1)
                       (apply ,fn (vector->list ,sym1))
                       (,fn ,sym1))))))

;;shorthand for (js-close (lambda (...) ...))
(define-macro (jlambda params . body)
  `(js-close (lambda ,params ,@body)))


(import-js-symbols biwa_interpreter)
(define (biwa-log x)
  (console-log x)
  (: biwa_interpreter 'log x)
  x)


;;;;;;;;;;;;;;;
;; ;apropos ;;;
;;;;;;;;;;;;;;;
(print "loading apropos")

;; list almost all symbols in the toplevel
(define (oblist)
  (let ((top  (: Object 'keys (<o> 'TopEnv  BiwaScheme)))
	(core (: Object 'keys (<o> 'CoreEnv BiwaScheme))))
    (js-array->list (: top 'concat core))))

(define *oblist* '())

;; apropos: search oblist
(define (apropos sym . fresh)
  (if (or (null? *oblist*) (car (list-pure fresh)))
      (set! *oblist* (oblist)))
  (let ((str (if (symbol? sym) (symbol->string sym) sym)))
    (filter (lambda (fname)
              (> (: fname 'search str) -1))
            *oblist*)))


(define-macro (comment . xs)
  ())

;;;;; example html generation
(comment (element-insert!
  (getelem "#content")
  (element-new `(ul
                 ,@(map (^ (message)
                           `(li class "list-elem"
                                ,message))
                        '("testing" "html" "generation"))))))
(define-macro (json x)
  `(: JSON 'stringify ,x))


(define (falsy? x)
  (or (js-undefined? x)
      (js-null? x)
      (null? x)
      (not x)))


(define (comp<o> . xs)
  (lambda (obj)
    (fold-left (^ (acc k) (<o> k acc)) obj xs)))

(define (atom->string x)
  (cond ((symbol? x) (symbol->string x))
        ((number? x) (number->string x))
        ((boolean? x) (if x "true" "false"))
        ((string? x) x)
        ((falsy? x) "")
        (else (raise (list->vector (list "not an atom: " (json x)))))))


(define (qsel q)
  (: document 'querySelector q))

(define (qsel-all q)
  (vector->list (: ((comp<o> 'Array 'prototype 'slice) window) 'call (: document 'querySelectorAll q))))


(define idle #f)
(call/cc (lambda (k) (set! idle k)))

(define (wait-for n)
  (call/cc
   (lambda (k)
     (timer (^ () (k)) n)
     (idle))))

(define (wait-for* n)
  (call/cc
   (lambda (k)
     (timer (^ (_) (k n)) n)
     (idle))))
  

(define-macro (w/js-cont continuation . body)
  (let ((cont (gensym))
        (kont (gensym)))
    `(lambda (,cont)
       (let ((,kont (symbol->string (gensym))))
         (js-set! window ,kont (js-close ,cont))
         (let ((,continuation (js-eval (string-append
                                        "(x)=>biwa_interpreter.invoke_closure("
                                        ,kont
                                        ", [x])"))))
           ,@body)))))



(define (compose-c/cc . fs)
  (let* ((is   (iota (length fs)))
	 (vals (list->vector (map (^ (x) #f) is)))
	 (cont #f))
    (list->vector
     (map (^ (f i)
	     (let1 v (call/cc f)
		   (vector-set! vals i v)
		   (biwa-log vals)
		   (when cont (cont))))
	  fs is))
    (call/cc (lambda (k) (set! cont k)))
    (vector->list vals)))

(define (js-cont-multiplify k)
  (js-call (js-eval "(f)=>((...xs)=>f(xs))") k))

(define (fetch-json url . opts)
  (w/js-cont kont
          (: (: (: window 'fetch url (alist->js-obj opts))
                'then (js-eval "(x)=>x.json()"))
             'then kont)
          (idle)))

(define (target-value e) (.. e 'target 'value))

(define (listen-change selector . accessor)
  (lambda (k)
    (let ((getval (if (null? accessor) target-value
		      (car accessor))))
	  (add-handler! selector "change"
			(lambda (e)
			  (k (getval e))))
	  (add-handler! selector "keyup"
			(lambda (e)
			  (wait-for 1) ;;cludge
			  (k (getval e))))
	  (getval (js-obj "target" (qsel selector))))))


(define-macro (build-query . keys)
  `(: (list->vector
       (list ,@(map (^ (key) `(string-append (symbol->string ',key) "=" (atom->string ,key))) keys)))
      'join "&"))


(define (js-map+ obj . xs)
  (if (null? xs)
      obj
      (begin (js-set! obj (atom->string (car xs)) (cadr xs))
             (apply js-map+ (cons obj (cddr xs))))))


(define (js-map . xs)
  (apply js-map+ (cons (js-obj) xs)))


(define (vec->lis vec)
  (vector->list (: (.. Array 'prototype 'slice) 'call vec)))

;;document level event handler
(define (listen-document-event event class)
  (call/cc
   (lambda (k)
     (add-handler! document event
                   (lambda (e)
                     (when (member class (vec->lis (.. e 'target 'classList)))
                           (k e))))
     (idle))
   'listen-document-event))

(define (listen-document-event* event class)
  (lambda (k)
    (add-handler!
     document event
     (lambda (e)
       (when (member class (vec->lis (.. e 'target 'classList)))
	     (k e))))
    (idle)))

(define (pick-alist alist . keys)
  (filter (^ (pair) (member (car pair) keys))
          alist))

(define (alist-edit alist . edits)
  (map (^ (pair)
          (let ((edit (assoc (car pair) edits)))
            (cons (car pair)
                  (if edit ((cdr edit) (cdr pair))
                      (cdr pair)))))
       alist))

(define (alist-sub-keys alist . subs)
  (map (^ (pair)
          (let1 sub (assoc (car pair) subs)
                (cons (if sub (cdr sub)
                          (car pair))
                      (cdr pair))))
       alist))


(define (encode-form obj)
  (apply string-append
         (map
          (lambda (pair)
            (format "~a=~a~%" (car pair) (encodeURI (atom->string (cdr pair)))))
          (js-obj->alist obj))))


(define-macro (maybe expr)
  (let ((k (gensym))
        (x (gensym)))
    `(call/cc
      (lambda (,k)
        (let1 ,x ,expr
              (if (falsy? ,x) (idle)
                  (,k ,x)))
        (idle))
      'maybe)))

(define  (post-text target body)
  (call/cc (fetch-json target
                       '("method" . "post")
                       `("headers" . ,(js-obj "Content-Type" "text/plain"))
                       `("body" . ,body))
                'post-text))


;; turn stuff into list
(define (->l x)
  (cond ((list? x) x)
        ((vector? x) (vector->list x))
        (else (js-obj->alist x))))


(define (map-keys fn . objs)
  (apply map (cons fn (map (^ (o) (->l (: Object 'keys o))) objs))))


(define-macro (1+ x)
  `(+ ,x 1))
(define-macro (1- x)
  `(- ,x 1))

(define (parse-json str)
  (if (falsy? str) "{}"
      (: JSON parse str)))


(define-macro (truthy? x)
  `(not (falsy? ,x)))

(define-macro (default x y)
  `(if (or (falsy? ,x) (equal? "" ,x)) ,y ,x))
