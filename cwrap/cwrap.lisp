; (defpackage :cwrap)

(in-package :om)

; define the dynamic library
(cffi::define-foreign-library libomcwrap
   (t (:default "/Applications/OPENMUSIC/packages/cwrap/libOmCwrap")))
   ; ( 
   ; 	(:macosx (om-relative-path nil "libOmCwrap") ) )
   ; 	(t (:default "libomspat"))
   ; )


(compile&load (om-relative-path '("src") "cwrap-src"))
; #+cwrap(compile&load (om-relative-path '("src") "cwrap-src"))

(compile&load (om-relative-path nil "cwrap-pack"))