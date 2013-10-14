

CL-SLP
=========

Common Lisp CFFI bindings to the OpenSLP library. Used for discovering and advertising 
services over the Service Location Protocol (SLP). 

Usage
------

CL-SLP just wraps the OpenSLP functions, see http://www.openslp.org/doc/html/ProgrammersGuide/index.html
for the official API reference.

Before loading the library, ensure your libslp.so (unix) or slp.dll (windows) library
is available to be loaded on your system. CL-SLP automatically pushes "C:/program files (x86)/OpenSLP/" onto
*foreign-library-directories* which should enable it to be loaded on Windows.

* Call SLP-OPEN before any other functions.

* Call SLP-CLOSE when finished using the library, this frees the memory allocated both by OpenSLP and CL-SLP

* Discover services using SLP-FIND-SERVERS 
This returns a list of discovered service urls

* Get service attributes using SLP-FIND-ATTRIBUTES
This returns a list of assoc lists for attributes of the given server

* Get available scopes using SLP-FIND-SCOPES

* Get discovered server types using SLP-FIND-SERVER-TYPES
This returns a list of discovered server types. These can be used as input to SLP-FIND-SERVERS

* Register a service using SLP-REGISTER

* Deregister a service using SLP-DEGREGISTER

* Delete a service attributes using SLP-DELETE-ATTRIBUTES

Other utility functions provided by OpenSLP are

* Parse service urls using SLP-PARSE-URL

* Correctly escape SLP strings using SLP-ESCAPE

* Unescape SLP strings using SLP-UNESCAPE

* Form a service url using SLP-SERVICE-URL
This takes the service type (e.g. "wbem:https")  and the address (e.g. "localhost:5989") and 
returns the correctly formatted url "service:wbem:https://localhost:5898"

* Form an SLP attribute string from an assoc list of attributes. Note that since SLP attributes
can take a list of values the assoc list can map attribute names to either atoms or lists .e.g.
(slp-format-attributes '((:a . 123) (:b 321) (:c 123 456)) -> "(A=123),(B=321),(C=123,456)"


Notes
------

The OpenSLP library makes extensive use of callbacks in its API. CL-SLP defines a default 
callback for each of the library calls, which it uses to collect return data. Users of CL-SLP
may if they wish define their own callbacks using the macros 
DEFINE-SERVER-TYPE-CALLBACK, DEFINE-SERVER-URL-CALLBACK, DEFINE-ATTR-CALLBACK and DEFINE-REGISTER-CALLBACK.

They can be called by passing the name of the new callback with the :callback-name keyword parameter to 
the functions that take it. The default behaviour should be sufficient for most needs, however. 

Note also that you MUST use a Lisp that supports callbacks; CL-SLP was developed and tested using SBCL
on Ubuntu Linux and Windows 7.

* Error -19 NETWORK_TIMED_OUT
This error code seems to be returned on Windows 7 machines (possibly others) on SLP-REGISTER and
SLP-DEREGISTER, even though the call appears to be successul. Services registered are discsoverable
and can be deregistered again so appear to be working fine, even though these calls error.
CL-SLP therefore ignores this error, but prints a message to *error-output*.


Example
--------

* Find services
(slp-find-server-types)
--> ("service:wbem:https")

* Register a service
(slp-register (slp-format-url "myservice.x" "localhost:8000"))
--> T

* Find services 
(slp-find-servers "myservice.x")
--> ("service:myservice.x://localhost:8000")

* Deregister service
(slp-deregister (slp-format-url "myservice.x" "localhost:8000"))
--> T

Note that the OpenSLP daemon (slpd) must be running for register/deregister functions to work.


Frank James
2013

