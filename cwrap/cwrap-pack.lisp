(in-package :om)

(let ((soundpack (omNG-make-package "Cwrap"
                   :container-pack *om-package-tree*
                   :doc "Examples of how to use C compiled methods in OM."
                   :classes '()
                   :functions '( hello-world wrap-number wrap-list wrap-sound )
                   )))
  )