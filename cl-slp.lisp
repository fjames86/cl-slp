
(defpackage :cl-slp
  (:use :cl :cffi)
  (:export #:slp-open
		   #:slp-close

		   ;; internal 
		   #:slp-find-scopes
		   #:slp-get-property
		   #:slp-get-properties
		   #:slp-get-refresh-interval

		   ;; discovery
		   #:slp-find-servers
		   #:slp-find-all-servers
		   #:slp-find-server-types
		   #:slp-find-attributes

		   ;; registration
		   #:slp-register
		   #:slp-deregister

		   ;; utils
		   #:slp-escape
		   #:slp-unescape
		   #:slp-parse-url
		   #:slp-format-url
		   #:slp-format-attributes

		   ;; common lisp error type
		   #:slp-error

		   ;; macros for defining callbacks
		   ;; users shouldn't need these, don't export?
		   #:define-server-type-callback
		   #:define-server-url-callback
		   #:define-attr-callback
		   #:define-register-callback

		   ))

(in-package :cl-slp)

;; ------

;; push a default path onto the foreign library dir for windows users
(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (member :windows *features*)
    (pushnew "C:/Program Files (x86)/OpenSLP/" *foreign-library-directories*
	     :test #'string-equal)))

(define-foreign-library libslp
  (:unix (:or "libslp.so" "libslp.so.1"))
  (:windows "slp.dll")
  (t (:default "libslp")))

(use-foreign-library libslp)

;; --------------- utils ---------


(defun split-string (string &optional (split-char #\space))
  (flet ((delimiterp (char)
		   (char= char split-char)))
    (loop for beg = 0 then (position-if-not #'delimiterp string :start (1+ end))
       for end = (when beg 
				   (position-if #'delimiterp string :start beg))
       collect (subseq string beg end)
       while end)))

(defun split-attribute-string (string)
  "Split an attribute string into an assoc list. Attr strings are formatted
as (name=value),(name=val1,val2,val3), i.e. comma seperated lists that map names to lists of values"
  (loop for beg = 0 then (position-if (lambda (c)
										(char= c #\())
									  string
									  :start (1+ end))
     for end = (when beg
				 (position-if (lambda (c)
								(char= c #\)))
							  string
							  :start (1+ beg)))
     nconc (if (and beg end)
			   (let* ((attr-str (subseq string (1+ beg) end))
					  (p (position #\= attr-str :test #'char=)))
				 (list (cons (intern (subseq attr-str 0 p) "KEYWORD")
							 (split-string (subseq attr-str (1+ p)) #\,)))))
     while (and beg end)))


;; ----- types -----------------

(defctype slp-error-type :int)

(defctype slp-handle :pointer)

(defctype slp-bool :boolean)

(defcstruct slp-server-url
  (server-type :string)
  (host :string)
  (port :int)
  (net-family :string)
  (server-part :string))

(defctype slp-server-type-callback :pointer)

;; ---- callback defining macros -------------

(defmacro define-server-type-callback
    (name (handle server-types error-code cookie) &body body)
  `(defcallback ,name slp-bool
       ((,handle slp-handle)
		(,server-types :string)
		(,error-code slp-error-type)
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
		(,error-code slp-error-type)
		(,cookie :pointer))
     (declare (ignorable ,handle ,url ,lifetime ,error-code ,cookie))
     ,@body))

(defctype slp-attr-callback :pointer)

(defmacro define-attr-callback
    (name (handle attr-list error-code cookie) &body body)
  `(defcallback ,name slp-bool
       ((,handle slp-handle)
		(,attr-list :string)
		(,error-code slp-error-type)
		(,cookie :pointer))
     (declare (ignorable ,handle ,attr-list ,error-code ,cookie))
     ,@body))

(defctype slp-register-callback :pointer)

(defmacro define-register-callback
    (name (handle error-code cookie) &body body)
  `(defcallback ,name :void
       ((,handle slp-handle)
		(,error-code slp-error-type)
		(,cookie :pointer))
     (declare (ignorable ,handle ,error-code ,cookie))
     ,@body))


;; ------- errors -------------

(define-condition slp-error (error)
  ((code :initarg :code :initform 0 :reader slp-error-code)
   (message :initarg :message :reader slp-error-message))
  (:report (lambda (err stream)
			 (format stream "SLP-ERROR ~A: ~A"
					 (slp-error-code err)
					 (slp-error-message err)))))

(defparameter *slp-error-messages*
  '((0 . "OK")
    (-1 . "Language not supported")
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

(defun slp-raise-error (error-code)
  (if (< error-code 0)
      (error 'slp-error
	     :code error-code
	     :message (let ((m (assoc error-code *slp-error-messages*)))
			(if m
			    (cdr m)
			    "Unknown Error")))
      t))

;; ------------- foreign calls ---------

;; open

(defparameter *slp-handle* nil)

(defcfun ("SLPOpen" %slp-open) slp-error-type
  (lang :string)
  (is-async slp-bool)
  (handle (:pointer slp-handle)))

(defun slp-open (&key (locale "en") is-async)
  "Initialise access to the OpenSLP library"
  (unless *slp-handle*
    (let ((handle (foreign-alloc 'slp-handle)))
      (with-foreign-string (lang locale)
		(let ((ret-code (%slp-open lang is-async handle)))
		  (setf *slp-handle* handle)
		  (slp-raise-error ret-code))))))

;; close

(defcfun ("SLPClose" %slp-close) :void
  (handle slp-handle))

(defun slp-close ()
  (when *slp-handle*
    (%slp-close (get-handle))
    (foreign-free *slp-handle*)
    (setf *slp-handle* nil)
    nil))

;;

(defun get-handle ()
  "Get the handle currently in use by cl-slp"
  (if *slp-handle*
      (mem-ref *slp-handle* 'slp-handle)
      (error 'slp-error
	     :code ""
	     :message "SLP not opened, call OPEN-SLP first")))

;; slp free

(defcfun ("SLPFree" %slp-free) :void
  (mem :pointer))

(defun slp-free (pointer)
  "Used to free any memory allocated by OpenSLP"
  (%slp-free pointer)
  nil)

;; find scopes

(defcfun ("SLPFindScopes" %slp-find-scopes) slp-error-type
  (handle slp-handle)
  (scope-list (:pointer :string)))

(defun slp-find-scopes ()
  "Set the list of scopes"
  (with-foreign-object (scopes :pointer)
    (let ((error-code (%slp-find-scopes (get-handle)
										scopes)))
      (if (zerop error-code)
		  (prog1
			  (mem-ref scopes :string)
			(slp-free (mem-ref scopes :pointer)))
		  (slp-raise-error error-code)))))

;; get property

(defcfun ("SLPGetProperty" %slp-get-property) :string
  (name :string))

(defun slp-get-property (name)
  "Get an SLP property. See *slp-properties* for a list of valid properties"
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

(defun slp-get-properties ()
  "Get all SLP properties as an assoc list"
  (mapcar (lambda (name)
	    (cons (intern name "KEYWORD")
		  (slp-get-property name)))
	  *slp-properties*))

;; set property

(defcfun ("SLPSetProperty" %slp-set-property) :void
  (name :string)
  (value :string))

(defun slp-set-property (name value)
  "Set an SLP property. This function is ignored by OpenSLP"
  (format *error-output* "WARNING: OpenSLP ignores calls to SLPSetProperty")

  (with-foreign-string (n name)
    (with-foreign-string (v value)
      (%slp-set-property n v))))

;; get refresh interval

(defcfun ("SLPGetRefreshInterval" %slp-get-refresh-interval) :unsigned-short)

(defun slp-get-refresh-interval ()
  (%slp-get-refresh-interval))

;; find servers

(defcfun ("SLPFindSrvs" %slp-find-servers) slp-error-type
  (handle slp-handle)
  (server-type :string)
  (scope-list :string)
  (filter :string)
  (callback slp-server-url-callback)
  (cookie :pointer))

(defparameter *default-server-url-callback* 'default-server-url-callback)

(defparameter *default-server-url-list* nil)

(define-server-url-callback default-server-url-callback
    (handle url lifetime error-code cookie)
  (when url
    (push url *default-server-url-list*))
  t)

(defun slp-find-servers (server-type
						 &key (callback-name *default-server-url-callback*)
						 (scope-list "") (filter "") cookie)
  "Returns a list of servers found of the specified type"
  (with-foreign-string (stype server-type)
    (with-foreign-string (slist scope-list)
      (with-foreign-string (f filter)
		(setf *default-server-url-list* nil)
		(let ((error-code
			   (%slp-find-servers (get-handle)
								  stype
								  slist
								  f
								  (get-callback callback-name)
								  (if cookie cookie (null-pointer)))))
		  (if (zerop error-code)
			  *default-server-url-list*
			  (slp-raise-error error-code)))))))

;; --- find server types

(defcfun ("SLPFindSrvTypes" %slp-find-server-types) slp-error-type
  (handle slp-handle)
  (naming-authority :string)
  (scope-list :string)
  (callback slp-server-type-callback)
  (cookie :pointer))

(defparameter *default-server-type-callback* 'default-server-type-callback)

(defparameter *default-server-type-list* nil)

(define-server-type-callback default-server-type-callback
    (handle server-types error-code cookie)
  (when server-types
    (let ((types (split-string server-types #\,)))
      (mapc (lambda (type)
			  (push type *default-server-type-list*))
			types)))
  t)

(defun slp-find-server-types (&key (callback-name *default-server-type-callback*)
							  (naming-authority "*") (scope-list "") cookie)
  "Returns a list of server types found"
  (with-foreign-string (nauth naming-authority)
    (with-foreign-string (slist scope-list)
      (setf *default-server-type-list* nil)
      (let ((error-code 
			 (%slp-find-server-types (get-handle)
									 nauth
									 slist
									 (get-callback callback-name)
									 (if cookie cookie (null-pointer)))))
		(if (zerop error-code)
			*default-server-type-list*
			(slp-raise-error error-code))))))

;; --------

;; find all servers

(defun slp-find-all-servers ()
  "Return a list of the urls of all servers found"
  (mapcan (lambda (type)
	    (slp-find-servers type))
	  (slp-find-server-types)))


;;; find attrs

(defcfun ("SLPFindAttrs" %slp-find-attributes) slp-error-type
  (handle slp-handle)
  (url :string)
  (scope-list :string)
  (attrib-ids :string)
  (callback slp-attr-callback)
  (cookie :pointer))

(defparameter *default-attr-callback* 'default-attr-callback)

(defparameter *default-attr-list* nil)

(define-attr-callback default-attr-callback
    (handle attr-list error-code cookie)
  (when attr-list
    ;; the attribute list is a comma seperated list with name=value pairs
    ;; (name=value), (name=value), ...
    (let ((alist (split-attribute-string attr-list)))
      (mapc (lambda (attr)
			  (when (not (assoc (car attr) *default-attr-list*))
				(push attr *default-attr-list*)))
			alist)))
  t)

(defun slp-find-attributes (url
							&key (callback-name *default-attr-callback*)
							(attrib-ids "") (scope-list "") cookie)
  "Retuns an assoc list of the attributes found for the url specified"
  (with-foreign-string (aids attrib-ids)
    (with-foreign-string (slist scope-list)
      (with-foreign-string (u url)
		(setf *default-attr-list* nil)
		(let ((error-code 
			   (%slp-find-attributes (get-handle)
									 u
									 slist
									 aids
									 (get-callback callback-name)
									 (if cookie cookie (null-pointer)))))
		  (if (zerop error-code)
			  *default-attr-list*
			  (slp-raise-error error-code)))))))

;;; -----------

(defcfun ("SLPReg" %slp-register) slp-error-type
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
  t)

(defparameter *default-lifetime* 10800)
(defparameter *maximum-lifetime* 65535)

(defun slp-register (url
					 &key (callback-name *default-register-callback*)
					 (lifetime *default-lifetime*)
					 (attributes "")
					 cookie)
  "Register a service with SLP. Needs slpd to be installed and running on the system"
  (with-foreign-string (u url)
    (with-foreign-string (a attributes)
      (let ((error-code 
			 (%slp-register (get-handle)
							u
							lifetime
							(null-pointer) ; server type is ignored
							a
							t ; fresh is always true
							(get-callback callback-name)
							(if cookie cookie (null-pointer)))))
		(cond
		  ((zerop error-code)
		   t)
		  ((= error-code -19)
		   (format *error-output* "WARNING: SLP-REGISTER returned error code NETWORK_TIMED_OUT~%")
		   t)
		  (t
		   (slp-raise-error error-code)))))))

;; ----- deregister 

(defcfun ("SLPDereg" %slp-deregister) slp-error-type
  (handle slp-handle)
  (url :string)
  (callback slp-register-callback)
  (cookie :pointer))

(defparameter *default-deregister-callback* 'default-deregister-callback)

(define-register-callback default-deregister-callback
    (handle error-code cookie)
  t)

(defun slp-deregister (url
					   &key (callback-name *default-deregister-callback*)
					   cookie)
  "Deregister the service"
  (with-foreign-string (u url)
    (let ((error-code
		   (%slp-deregister (get-handle)
							u
							(get-callback callback-name)
							(if cookie cookie (null-pointer)))))
      (cond
		((zerop error-code)
		 t)
		((= error-code -19)
		 (format *error-output* "WARNING: SLP-DEREGISTER returned error code NETWORK_TIMED_OUT~%")
		 t)
		(t
		 (slp-raise-error error-code))))))

;;;

(defcfun ("SLPParseSrvURL" %slp-parse-url) slp-error-type
  (url :string)
  (server-url (:pointer (:struct slp-server-url))))

(defun slp-parse-url (url)
  "Parse a service url"
  (with-foreign-string (u url)
    (with-foreign-object (server-url :pointer)
      (let ((error-code
			 (%slp-parse-url u server-url)))
		(if (zerop error-code)
			(prog1 (mapcar (lambda (name)
					 (cons (intern (symbol-name name) "KEYWORD")
					       (foreign-slot-value (mem-ref server-url :pointer)
								   '(:struct slp-server-url)
								   name)))
				       '(server-type host port server-part net-family))
			  (slp-free (mem-ref server-url :pointer)))
			(slp-raise-error error-code))))))


;;

(defcfun ("SLPEscape" %slp-escape) slp-error-type
  (unescaped :string)
  (escaped (:pointer :string))
  (istag slp-bool))

(defun slp-escape (str &optional istag)
  "Properly escape SLP strings"
  (with-foreign-string (un str)
    (with-foreign-object (es :pointer)
      (let ((error-code (%slp-escape un es istag)))
		(if (zerop error-code)
			(prog1
				(foreign-string-to-lisp (mem-ref es :pointer))
			  (slp-free (mem-ref es :pointer)))
			(slp-raise-error error-code))))))

(defcfun ("SLPUnescape" %slp-unescape) slp-error-type
  (escaped :string)
  (unescape (:pointer :string))
  (istag slp-bool))

(defun slp-unescape (str &optional istag)
  "Unescape SLP strings"
  (with-foreign-string (es str)
    (with-foreign-object (un :pointer)
      (let ((error-code (%slp-unescape es un istag)))
		(if (zerop error-code)
			(prog1
				(foreign-string-to-lisp (mem-ref un :pointer))
			  (slp-free (mem-ref un :pointer)))
			(slp-raise-error error-code))))))

(defun slp-format-url (service-type address)
  "Format an SLP service URL"
  (format nil "service:~A://~A" service-type address))

(defun slp-format-attributes (attribute-alist)
  "Format an assoc list of attributes into an slp attribute string"
  (with-output-to-string (s)
    (loop with printed = nil
	   for attr-pair in attribute-alist 
	   do 
		 (progn
		   (when printed
			 (format s ","))
		   (format s "(~A=" (car attr-pair))
		   (let ((vals (cdr attr-pair)))
			 (if (atom vals)
				 (format s "~A" vals)
				 (loop with pr = nil
					for val in vals
					do 
					  (progn
						(when pr
						  (format s ","))
						(format s "~A" val)
						(setf pr t)))))
		   (format s ")")
		   (setf printed t)))))
