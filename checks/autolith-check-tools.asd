;;;; -- Source Check Tools --

;;; qlot installs a project's dependencies by reading its system definitions.
;;; This system names the development tools the source checks load, so
;;; installing checks/qlfile also installs their own dependencies. Nothing
;;; loads it.

(asdf:defsystem #:autolith-check-tools
  :description "The development tools Autolith's source checks load."
  :depends-on  (#:mallet))
