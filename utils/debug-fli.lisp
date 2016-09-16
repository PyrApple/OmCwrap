(fli:define-c-struct  audiobuffer-struct
  ( numChannels :unsigned-int )
  ( numSamples  :unsigned-long )
  ( data        :pointer )
 )

(fli:define-c-typedef audiobuffer-type (:pointer (:struct audiobuffer-struct)))

(defun testwrapsound ()
(let* ( 
        (audiob  (fli::allocate-foreign-object :type 'audiobuffer-struct))
        (nch 3)
        (data-in (make-list 30000 :initial-element 0.0))
        (nsamp (length data-in))
      )

      (progn

        ;;; ALLOCATE MEMORY
        (format t  "allocating memory..~%")

        (setf (fli::foreign-slot-value audiob 'numChannels :object-type  'audiobuffer-struct) nch)
        (format t  "  numChannels allocated: ~D~%" nch)

        (setf (fli::foreign-slot-value audiob 'numSamples :object-type  'audiobuffer-struct) nsamp)
        (format t  "  numSamples allocated: ~D~%" nsamp)

        (setf (fli:foreign-slot-value audiob 'audiobuffer-struct 'data) 
                 (fli::allocate-foreign-object :type :pointer 
                  :nelems nch
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
          (let  ((channelpointer ( fli::allocate-foreign-object :unsigned-int :nelems nsamp)))
                (setf (fli::dereference (fli:foreign-slot-value audiob 'audiobuffer-struct 'data) :pointer c) channelpointer)
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
        
        (fli::free-foreign-object audiob )
        (format t  "  audiob freed~%")
      )
  )
)

(loop for c from 0 to 3 do (format t  "~%=== ITER: ~D ===~%" c) (testwrapsound))
(format t  "~%-> DONE~%")