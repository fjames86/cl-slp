
(defpackage :cl-slp
  (:use :cl :cffi)
  (:export #:slp-open
		   #:slp-close
		   #:slp-find-scopes
		   #:slp-get-property
		   #:slp-set-property
		   #:slp-get-refresh-interval
		   #:slp-find-servers
		   #:slp-find-server-types
		   #:slp-find-attributes
		   #:slp-register
		   #:slp-deregister
		   #:slp-delete-attributes
		   #:slp-escape
		   #:slp-unescape
		   #:slp-parse-url
		   #:slp-service-url
		   
		   #:define-server-type-callback
		   #:define-server-url-callback		   
		   #:define-attr-callback
		   #:define-register-callback))

(in-package :cl-slp)

;; ------

(define-foreign-library libslp
  (:unix (:or "libslp.so" "libslp.so.1"))
  (:windows "libslp.dll")
  (t (:default "libslp")))

(use-foreign-library libslp)


;; ----- types -----------------

(defctype slp-error :int)

(defctype slp-handle :pointer)

(defctype slp-bool :boolean)

(defcstruct slp-server-url
  (server-type :string)
  (host :string)
  (port :int)
  (net-family :string)
  (server-part :string))

(defctype slp-server-type-callback :pointer)

(defmacro define-server-type-callback
	(name (handle server-types error-code cookie) &body body)
  `(defcallback ,name slp-bool
	   ((,handle slp-handle)
		(,server-types :string)
		(,error-code slp-error)
		(,cookie :pointer))
	 (declare (ignorable ,handle ,server-types ,error-code ,cookie))
	 ,@body))

(defctype slp-server-url-callback :pointer)

(defmacro define-server-url-callback
	(name (handle url lifetime error-code cookie) &body body)
  `(defcallback ,name slp-bool
	   ((,handle slp-handle)
		(,url :string)
		(,lifetime :unsigned-short)
		(,error-code slp-error)
		(,cookie :pointer))
	 (declare (ignorable ,handle ,url ,lifetime ,error-code ,cookie))
	 ,@body))

(defctype slp-attr-callback :pointer)

(defmacro define-attr-callback
	(name (handle attr-list error-code cookie) &body body)
  `(defcallback ,name slp-bool
	   ((,handle slp-handle)
		(,attr-list :string)
		(,error-code slp-error)
		(,cookie :pointer))
	 (declare (ignorable ,handle ,attr-list ,error-code ,cookie))
	 ,@body))

(defctype slp-register-callback :pointer)

(defmacro define-register-callback
	(name (handle error-code cookie) &body body)
  `(defcallback ,name :void
	   ((,handle slp-handle)
		(,error-code slp-error)
		(,cookie :pointer))
	 (declare (ignorable ,handle ,error-code ,cookie))
	 ,@body))


;; ------- errors -------------

(define-condition slp-condition (error)
  ((:code :initarg :code :initform 0 :reader slp-condition-code)
   (:message :initarg :message :reader slp-condition-message))
  (:report (lambda (condition stream)
			 (format stream "SLP-ERROR ~A: ~A"
					 (slp-condition-code condition)
					 (slp-condition-message condition)))))

(defparameter *slp-error-messages*
  '((0 . "OK")
	(-1 . "Language no supported")
	(-2 . "Parse error")
	(-3 . "Invalid registration")
	(-4 . "Scope not supported")
	(-6 . "Authentication absent")
	(-7 . "Authentication failed")
	(-13 . "Invalid update")
	(-15 . "Refresh rejected")
	(-17 . "Not Implemented")
	(-18 . "Buffer overflow")
	(-19 . "Network timed out")
	(-20 . "Network init failed")
	(-21 . "Memory alloc failed")
	(-22 . "Parameter bad")
	(-23 . "Network error")
	(-24 . "Internal system error")
	(-25 . "Handle in use")
	(-26 . "Type error")
	(-27 . "Retry unicast")))

(defmacro slp-error (&body body)
  (let ((gerr (gensym "ERROR-CODE")))
	`(let ((,gerr (progn ,@body)))
	   (if (< ,gerr 0)
		   (error 'slp-condition
				  :code ,gerr
				  :message (let ((m (assoc ,gerr *slp-error-messages*)))
							 (if m (cdr m) "Unknown error")))
		   t))))
		     
;; ------------- foreign calls ---------

;; open

(defparameter *slp-handle* nil)

(defcfun ("SLPOpen" %slp-open) slp-error
  (lang :string)
  (is-async slp-bool)
  (handle (:pointer slp-handle)))

(defun slp-open (&optional is-async)
  (unless *slp-handle*
	(let ((handle (foreign-alloc 'slp-handle)))
	  (with-foreign-string (lang "en")
		(let ((ret-code (%slp-open lang is-async handle)))
		  (setf *slp-handle* handle)
		  (slp-error ret-code))))))

;; close

(defcfun ("SLPClose" %slp-close) :void
  (handle slp-handle))

(defun slp-close ()
  (when *slp-handle*
	(%slp-close (mem-ref *slp-handle* 'slp-handle))
	(foreign-free *slp-handle*)
	(setf *slp-handle* nil)
	nil))

;;

(defun get-handle ()
  (if *slp-handle*
	  (mem-ref *slp-handle* 'slp-handle)
	  (error 'slp-condition
			 :message "SLP not opened, call OPEN-SLP first")))

;; slp free

(defcfun ("SLPFree" %slp-free) :void
  (mem :pointer))

(defun slp-free (pointer)
  (%slp-free pointer)
  nil)

;; find scoopes

(defcfun ("SLPFindScopes" %slp-find-scopes) slp-error
  (handle slp-handle)
  (scope-list (:pointer :string)))

(defun slp-find-scopes ()
  (with-foreign-object (scopes :pointer)
	(let ((error-code (%slp-find-scopes (get-handle)
										scopes)))
	  (if (zerop error-code)
		  (prog1
			(mem-ref scopes :string)
			(slp-free (mem-ref scopes :pointer)))
		  (slp-error error-code)))))
	  
;; get property

(defcfun ("SLPGetProperty" %slp-get-property) :string
  (name :string))

(defun slp-get-property (name)
  (with-foreign-string (n name)
	(%slp-get-property n)))

(defparameter *slp-properties*
  (list "net.slp.useScopes"
		"net.slp.DAAddresses"
		"net.slp.interfaces"
		"net.slp.broadcastAddr"
		"net.slp.isBroadcastOnly"
		"net.slp.passiveDADetection"
		"net.slp.DAActiveDiscoveryInterval"
		"net.slp.multicastTTL"
		"net.slp.multicastMaximumWait"
		"net.slp.unicastMaximumWait"
		"net.slp.randomWaitBound"
		"net.slp.MTU"
		"net.slp.interfaces"
		"net.slp.securityEnabled"
		"net.slp.locale"
		"net.slp.maxResults"
		"net.slp.isDA"
		"net.slp.DAHeartBeat"
		"net.slp.DAAttributes"
		"net.slp.useIPV4"
		"net.slp.useIPV6"))

		
;; set property

(defcfun ("SLPSetProperty" %slp-set-property) :void
  (name :string)
  (value :string))

(defun slp-set-property (name value)
  (with-foreign-string (n name)
	(with-foreign-string (v value)
	  (%slp-set-property n v))))

;; get refresh interval

(defcfun ("SLPGetRefreshInterval" %slp-get-refresh-interval) :unsigned-short)

(defun slp-get-refresh-interval ()
  (%slp-get-refresh-interval))

;; find servers

(defcfun ("SLPFindSrvs" %slp-find-servers) slp-error
  (handle slp-handle)
  (server-type :string)
  (scope-list :string)
  (filter :string)
  (callback slp-server-url-callback)
  (cookie :pointer))

(defparameter *default-server-url-callback* 'default-server-url-callback)

(define-server-url-callback default-server-url-callback
	(handle url lifetime error-code cookie)
  (format t "Url: ~A~%" url)
  t)

(defun slp-find-servers (server-type
						 &key (callback-name *default-server-url-callback*)
						 (scope-list "") (filter "") cookie)
  (with-foreign-string (stype server-type)
	(with-foreign-string (slist scope-list)
	  (with-foreign-string (f filter)
		(slp-error 
		  (%slp-find-servers (get-handle)
							 stype
							 slist
							 f
							 (get-callback callback-name)
							 (if cookie cookie (null-pointer))))))))

;; --- find server types

(defcfun ("SLPFindSrvTypes" %slp-find-server-types) slp-error
  (handle slp-handle)
  (naming-authority :string)
  (scope-list :string)
  (callback slp-server-type-callback)
  (cookie :pointer))

(defparameter *default-server-type-callback* 'default-server-type-callback)

(define-server-type-callback default-server-type-callback
	(handle server-types error-code cookie)
  (format t "Server-types: ~A~%" server-types)
  t)

(defun slp-find-server-types (&key (callback-name *default-server-type-callback*)
							  (naming-authority "") (scope-list "") cookie)
  (with-foreign-string (nauth naming-authority)
	(with-foreign-string (slist scope-list)
	  (slp-error
		(%slp-find-server-types (get-handle)
								nauth
								slist
								(get-callback callback-name)
								(if cookie cookie (null-pointer)))))))

;;; find attrs

(defcfun ("SLPFindAttrs" %slp-find-attributes) slp-error
  (handle slp-handle)
  (url :string)
  (scope-list :string)
  (attrib-ids :string)
  (callback slp-attr-callback)
  (cookie :pointer))

(defparameter *default-attr-callback* 'default-attr-callback)

(define-attr-callback default-attr-callback
	(handle attr-list error-code cookie)
  (format t "Attr-list: ~A~%" attr-list)
  t)

(defun slp-find-attributes (url
							&key (callback-name *default-attr-callback*)
							(attrib-ids "") (scope-list "") cookie)
  (with-foreign-string (aids attrib-ids)
	(with-foreign-string (slist scope-list)
	  (with-foreign-string (u url)
		(slp-error
		  (%slp-find-attributes (get-handle)
								u
								slist
								aids
								(get-callback callback-name)
								(if cookie cookie (null-pointer))))))))

;;; -----------

(defcfun ("SLPReg" %slp-register) slp-error
  (handle slp-handle)
  (url :string)
  (lifetime :unsigned-short)
  (server-type :string)
  (attributes :string)
  (fresh slp-bool)
  (callback slp-register-callback)
  (cookie :pointer))

(defparameter *default-register-callback* 'default-register-callback)

(define-register-callback default-register-callback
	(handle error-code cookie)
  (format t "Register: ~A~%" error-code)
  t)

(defparameter *default-lifetime* 10800)
(defparameter *maximum-lifetime* 65535)

(defun slp-register (url
					 &key (callback-name *default-register-callback*)
					 (lifetime *default-lifetime*)
					 (attributes "")
					 cookie)
  (with-foreign-string (u url)
	(with-foreign-string (a attributes)
	  (slp-error
		(%slp-register (get-handle)
					   u
					   lifetime
					   (null-pointer) ; server type is ignored
					   a
					   t ; fresh is always true
					   (get-callback callback-name)
					   (if cookie cookie (null-pointer)))))))

;; ----- deregister 

(defcfun ("SLPDereg" %slp-deregister) slp-error
  (handle slp-error)
  (url :string)
  (callback slp-register-callback)
  (cookie :pointer))

(defparameter *default-deregister-callback* 'default-deregister-callback)

(define-register-callback default-deregister-callback
	(handle error-code cookie)
  (format t "Deregister: ~A~%" error-code)
  t)

(defun slp-deregister (url
					   &key (callback-name *default-deregister-callback*)
					   cookie)
  (with-foreign-string (u url)
	(slp-error
	  (%slp-deregister (get-handle)
					   u
					   (get-callback callback-name)
					   (if cookie cookie (null-pointer))))))

;;

(defcfun ("SLPDelAttrs" %slp-del-attrs) slp-error
  (handle slp-handle)
  (url :string)
  (attrs :string)
  (callback slp-register-callback)
  (cookie :pointer))

(defparameter *default-del-attr-callback* 'default-del-attr-callback)

(define-register-callback default-del-attr-callback
	(handle error-code cookie)
  (format t "Del attr: ~A~%" error-code)
  t)

(defun slp-delete-attributes (url attrs
							  &key (callback-name *default-del-attr-callback*)
							  cookie)
  (with-foreign-string (u url)
	(with-foreign-string (a attrs)
	  (slp-error
		(%slp-del-attrs (get-handle)
						u
						a
						(get-callback callback-name)
						(if cookie cookie (null-pointer)))))))

;;;

(defcfun ("SLPParseSrvURL" %slp-parse-url) slp-error
  (url :string)
  (server-url (:pointer (:struct slp-server-url))))

(defun slp-parse-url (url)
  (with-foreign-string (u url)
	(with-foreign-object (server-url :pointer)
	  (let ((error-code
			 (%slp-parse-url u server-url)))
		(if (zerop error-code)
			(prog1
				(list (cons :server-type
							(foreign-slot-value (mem-ref server-url :pointer)
												'(:struct slp-server-url)
												'server-type))
					  (cons :host
							(foreign-slot-value (mem-ref server-url :pointer)
												'(:struct slp-server-url)
												'host))
					  (cons :port
							(foreign-slot-value (mem-ref server-url :pointer)
												'(:struct slp-server-url)
												'port))
					  (cons :net-family
							(foreign-slot-value (mem-ref server-url :pointer)
												'(:struct slp-server-url)
												'net-family))
					  (cons :server-part
							(foreign-slot-value (mem-ref server-url :pointer)
												'(:struct slp-server-url)
												'server-part)))
			  (slp-free (mem-ref server-url :pointer)))
			(slp-error error-code))))))


;;

(defcfun ("SLPEscape" %slp-escape) slp-error
  (unescaped :string)
  (escaped (:pointer :string))
  (istag slp-bool))

(defun slp-escape (str &optional istag)
  (with-foreign-string (un str)
	(with-foreign-object (es :pointer)
	  (let ((error-code (%slp-escape un es istag)))
		(if (zerop error-code)
			(prog1
				(foreign-string-to-lisp (mem-ref es :pointer))
			  (slp-free (mem-ref es :pointer)))
			(slp-error error-code))))))

(defcfun ("SLPUnescape" %slp-unescape) slp-error
  (escaped :string)
  (unescape (:pointer :string))
  (istag slp-bool))

(defun slp-unescape (str &optional istag)
  (with-foreign-string (es str)
	(with-foreign-object (un :pointer)
	  (let ((error-code (%slp-unescape es un istag)))
		(if (zerop error-code)
			(prog1
				(foreign-string-to-lisp (mem-ref un :pointer))
			  (slp-free (mem-ref un :pointer)))
			(slp-error error-code))))))

(defun slp-service-url (service-type address)
  (format nil "service:~A://~A" service-type address))

