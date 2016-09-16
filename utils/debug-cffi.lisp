(load "~/quicklisp/setup.lisp")

(ql:quickload "cffi")

;;;======================================================================

(cffi:defcstruct audiobuffer-struct
  ( numChannels :unsigned-int )
  ( numSamples  :unsigned-int )
  ( data        :pointer )
 )

(cffi::defctype audiobuffer-type (:pointer (:struct audiobuffer-struct)))

(defun testwrapsound ()
(let* ( 
        (audiob  (cffi::foreign-alloc 'audiobuffer-type))
        (nch 30)
        (data-in (make-list 30000 :initial-element 0.0))
        (nsamp (length data-in))
      )

      (progn

        ;;; ALLOCATE MEMORY
        (format t  "allocating memory..~%")

        (setf (cffi:foreign-slot-value audiob '(:struct audiobuffer-struct) 'numChannels) nch)
        (format t  "  numChannels allocated: ~D~%" nch)

        (setf (cffi:foreign-slot-value audiob '(:struct audiobuffer-struct) 'numSamples) nsamp)
        (format t  "  numSamples allocated: ~D~%" nsamp)

        (setf (cffi:foreign-slot-value audiob '(:struct audiobuffer-struct) 'data) 
                 (cffi::foreign-alloc :pointer 
                  :count nch
                  ; :initial-contents (loop for c from 0 to (1- nch) 
                  ;   do (format t  "  pointer alloc to ch: ~D~%" c)
                  ;   collect (print ( cffi::foreign-alloc :float 
                  ;                                 :count nsamp 
                  ;                                 ; :initial-contents data-in
                  ;   )
                  ; ))
                )
        )
        (format t  "  data allocated~%")

        (loop for c from 0 to (1- nch) do
          (let  ((channelpointer ( cffi::foreign-alloc :unsigned-int :count nsamp)))
                (setf (cffi::mem-aref (cffi:foreign-slot-value audiob '(:struct audiobuffer-struct) 'data) :pointer c) channelpointer)
                (loop for s from 0 to (1- nsamp) do 
                  (setf 
                    (cffi::mem-aref channelpointer :unsigned-int s)
                    (coerce 0 'integer)
                  )
                )
        ))

        
        ;;; FREE MEMORY
        (format t  "freeing memory...~%")

        (dotimes (c nch)
          ; (fli::free-foreign-object (fli:dereference (cffi:foreign-slot-value audiob '(:struct audiobuffer-struct) 'data) :type :pointer :index c))
          (cffi::foreign-free (cffi::mem-aref (cffi:foreign-slot-value audiob '(:struct audiobuffer-struct) 'data) :pointer c))
          (format t  "  freed pointer to ch: ~D~%" c)
        )
        (format t  "  data content freed~%")
        
        ( cffi::foreign-free (cffi:foreign-slot-value audiob '(:struct audiobuffer-struct) 'data) )
        (format t  "  data freed~%")
        
        ( cffi::foreign-free audiob )
        (format t  "  audiob freed~%")
      )
  )
)

(loop for c from 0 to 30000 do (format t  "~%=== ITER: ~D ===~%" c) (testwrapsound))
(format t  "~%-> DONE~%")