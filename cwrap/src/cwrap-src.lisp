(in-package :om)

; load foreign library
(cffi::use-foreign-library libomcwrap)

;;;======================================================================
(cffi:defcfun  ("PrintChar" print-char-cfun) :string) ; return string

(defmethod* hello-world ( val )
  :icon '(347)
  :menuins '()
  :indoc '("")
  :outdoc '("")
  :initvals '(3)
  :doc "print a character in OM console from C library"

  ( print-char-cfun )
)

;;;======================================================================
(cffi:defcfun  ("WrapInt" wrap-int-cfun) :int ; return an int
	( val :int )
)

(cffi:defcfun  ("WrapUnsignedInt" wrap-unsigned-int-cfun) :unsigned-int ; return an unsigned int
  ( val :unsigned-int )
)

(cffi:defcfun  ("WrapFloat" wrap-float-cfun) :float ; return a float
  ( val :float )
)

(defmethod* wrap-number ( val )
  :icon '(347)
  :menuins '()
  :indoc '("number")
  :outdoc '("")
  :initvals '(-3)
  :doc "pass a number to C, catch returned number from C"
  (if (typep val 'integer) 
    (if (> val 0) 
      ( wrap-unsigned-int-cfun val ) 
      ( wrap-int-cfun val ) )
    (if (typep val 'float) 
        ( wrap-float-cfun val ) 
        ( print "non recognized number type" ) )
  )
)

;;;======================================================================
(cffi:defcstruct list-struct
  ( mySize :unsigned-int )
  ( myList :pointer )
)

(cffi::defctype list-type (:pointer (:struct list-struct)))

(cffi:defcfun  ("WrapList" wrap-list-cfun) :void
	( om-list-struct (:pointer (:struct list-struct) ) ) 
)

; convert lisp list to c-ready list structure, allocating foreign values
(defun allocate-list (list-in) 
  (let* ( (list-type-in (cffi::foreign-alloc 'list-type)) )
      (setf (cffi:foreign-slot-value list-type-in '(:struct list-struct) 'mySize) (length list-in))
      (setf (cffi:foreign-slot-value list-type-in '(:struct list-struct) 'myList) (cffi:foreign-alloc :float :initial-contents list-in))
      list-type-in
    )
)

; free pointer values allocated in allocate-list
(defun free-list (list-type-in)
    ( cffi::foreign-free (cffi:foreign-slot-value list-type-in '(:struct list-struct) 'myList) )
    ( cffi::foreign-free list-type-in )
)

(defmethod* wrap-list ( list-in )
  :icon '(347)
  :menuins '()
  :indoc '("lisp list")
  :outdoc '("")
  :initvals '( '(1.2 3.2 7.8) )
  :doc  "pass a list as a structure to C, return modified list"

  (let* ( (list-type-in (allocate-list list-in)) 
          (list-out nil) 
        )

        (progn ; force ordered evaluation to avoid freeing pointers before the end

          ; execute c function
          ( wrap-list-cfun list-type-in )
          
          ; get modified list (read n values from pointer)
          (setq list-out (loop for i below (length list-in) 
            collect (cffi::mem-aref (cffi:foreign-slot-value list-type-in '(:struct list-struct) 'myList) :float i) ) )

          ; free pointers
          (free-list list-type-in)

          ; return modified list
          (print list-out)
        )
  )
)

;;;======================================================================

(cffi:defcstruct audiobuffer-struct
  ( numChannels :unsigned-int )
  ( numSamples  :unsigned-long )
  ; ( interleaved :boolean )
  ( data 		:pointer )
 )

(cffi::defctype audiobuffer-type (:pointer (:struct audiobuffer-struct)))

(cffi:defcfun  ("WrapSound" wrap-buffer-cfun) :void ; return void
	( buffer (:pointer (:struct audiobuffer-struct) ) ) 
)


; allocate audio buffer structure fields from input om-sound data
(defun allocate-audio-buffer ( om-sound ) 
(let* ( (audiob (cffi::foreign-alloc 'audiobuffer-type)) 
        (sound-buffer (om-allocate-sound-buffer om-sound) ) )
    (progn
      (setf (cffi:foreign-slot-value audiob '(:struct audiobuffer-struct) 'numChannels) (n-channels om-sound))
      (setf (cffi:foreign-slot-value audiob '(:struct audiobuffer-struct) 'numSamples) (n-samples om-sound))
      (setf (cffi:foreign-slot-value audiob '(:struct audiobuffer-struct) 'data) sound-buffer )
      audiob
    )
))

; free audio buffer
(defun free-audio-buffer (audiob numCh) 
  (om-audio::om-free-sound-buffer (cffi:foreign-slot-value audiob '(:struct audiobuffer-struct) 'data) (- numCh 1 ) )
  ; (cffi::foreign-free (cffi:foreign-slot-value audiob '(:struct audiobuffer-struct) 'data))
  (cffi::foreign-free audiob)
)

; extract channel from om-sound as a list of float. returns said list
(defun om-sound-to-list ( om-sound channel )
  (with-audio-buffer (b om-sound)
    (let  ( (numdat (n-samples om-sound)) )
          
          (let ((channel-ptr (om-read-ptr (om-sound-buffer-ptr b) channel :pointer)))
          (loop for i from 0 to numdat collect
                (om-read-ptr channel-ptr i :float)))
        
    )))

; allocate and returns sound buffer. a sound buffer is used to fill the 'data field of an audio-buffer structure,
; being an array of arrays (one per channel in om-sound) that contains sound data as floats. (by array understand pointer)
(defun om-allocate-sound-buffer ( om-sound ) 
  (let* (
        (nch (n-channels om-sound) )
        (size (n-samples om-sound) ) )

        (fli::allocate-foreign-object 
          :type :pointer :nelems nch
          :initial-contents (loop for c from 0 to (1- nch) collect
            (fli::allocate-foreign-object :type :float :nelems size :initial-contents ( om-sound-to-list om-sound  c ) ))
        )))

; create and returns an om sound from the data in an audiobuffer-struct
(defun make-om-sound-instance-from-audio-buffer ( audiob ) 
(let* ( (n-channels-out (cffi:foreign-slot-value audiob '(:struct audiobuffer-struct) 'numChannels) )
        (n-samples-out (cffi:foreign-slot-value audiob '(:struct audiobuffer-struct) 'numSamples) )
        (out-buffer-data (cffi:foreign-slot-value audiob '(:struct audiobuffer-struct) 'data) )
        ; create an om-sound-buffer to which audiob data values will be copied
        (om-buffer-out (make-om-sound-buffer-gc 
                        :nch n-channels-out
                        :ptr (make-audio-buffer n-channels-out n-samples-out)))
      )
      
      ;fill in om sound buffer with data values from audio-buffer audiob
      (dotimes (c n-channels-out)
        (dotimes (smp n-samples-out)
          (setf (fli:dereference (fli:dereference (om-sound-buffer-ptr om-buffer-out) :index c :type :pointer)
                                 :index smp :type :float)
                (fli:dereference (fli:dereference out-buffer-data :index c :type :pointer) :index smp :type :float)
                )))

      ; create om-sound from filled buffer
      (om-init-instance (make-instance 'sound :buffer om-buffer-out
                                       :n-samples n-samples-out
                                       :n-channels n-channels-out
                                       :sample-rate 44100) ; (sample-rate om-sound-in)
                        `((:file , nil)))
))

(defmethod* wrap-sound ( om-sound-in )
  :icon '(347)
  :menuins '()
  :indoc '("om-sound")
  :outdoc '("")
  :initvals '( nil )
  :doc  "pass an OM sound as a structure to C, return modified sound"

  (let* ( (audiob (allocate-audio-buffer om-sound-in)) )

        (unwind-protect
          (progn 
            ; execute c function
            ( wrap-buffer-cfun audiob )
            ; get modified sound (create new om sound, fill it with audiob data values)
            ( make-om-sound-instance-from-audio-buffer audiob )
          )
          ; cleanup forms
          (free-audio-buffer audiob (n-channels om-sound-in) )
        )
  ))
