(in-package #:autolith)

;;;; -- Parinfer Indentation --

;;; Editors running parinfer-rust in its paren mode reindent any line whose
;;; indentation disagrees with the parentheses around it, and refuse files
;;; holding tabs. This rule runs the same library build, the parinfer-rust
;;; fork Emacs's parinfer-rust-mode downloads, with the options that mode uses
;;; for Lisp, so committed source is exactly what the editor expects.

(defparameter *source-parinfer-options*
  (list "lispVlineSymbols" t "lispBlockComments" t)
  "The parinfer options parinfer-rust-mode uses for Common Lisp buffers.")

(defvar *source-parinfer-loaded-library* nil
  "The pathname of the parinfer library loaded into this image, or NIL.")

(defclass source-parinfer-rule (source-rule)
  ((library
    :initarg       :library
    :reader        source-parinfer-rule-library
    :type          pathname
    :documentation "The private parinfer library ./script/check builds."))
  (:default-initargs
   :name "parinfer"
   :description "Indentation agrees with the parentheses as parinfer-rust's paren mode requires, so editors running parinfer leave files unchanged.")
  (:documentation "Source is a fixed point of parinfer's paren mode."))


;;;; -- Rule --

(defmethod source-rule-applies-p ((rule source-parinfer-rule) (file source-file))
  "Inspect Lisp source without tabs, which the tab rule reports, since parinfer refuses them."
  (declare (ignore rule))
  (and (source-file-lisp-p file)
       (not (find #\Tab (source-file-text file)))))

(defmethod source-rule-findings ((rule source-parinfer-rule) (file source-file))
  "Report a parinfer error, or each line paren mode would reindent."
  (let ((answer (source-parinfer-run (source-parinfer-rule-library rule)
                                     (source-file-text file))))
    (if (eq (json-get answer "success") t)
        (loop with fixed = (coerce (uiop:split-string (json-get answer "text")
                                                      :separator '(#\Newline))
                                   'simple-vector)
              for text across (source-file-lines file)
              for line from 1
              for wanted = (and (< (1- line) (length fixed)) (aref fixed (1- line)))
              unless (equal text wanted)
                collect (source-finding-create
                         rule file
                         :line    line
                         :message (format nil "Parinfer indents this line to column ~D."
                                   (or (position #\Space wanted :test-not #'char=) 0))
                         :column  (or (position #\Space text :test-not #'char=) 0)))
        (let ((error (json-get answer "error")))
          (list (source-finding-create
                 rule file
                 :line    (1+ (or (and error (json-get error "lineNo")) 0))
                 :message (format nil "Parinfer cannot read the file: ~A"
                           (or (and error (json-get error "message")) "unknown error"))
                 :column  (or (and error (json-get error "x")) 0)))))))

(defmethod source-rule-fix ((rule source-parinfer-rule) (file source-file))
  "Return the text paren mode produces, or NIL when parinfer cannot read the file."
  (let ((answer (source-parinfer-run (source-parinfer-rule-library rule)
                                     (source-file-text file))))
    (and (eq (json-get answer "success") t)
         (json-get answer "text"))))


;;;; -- Public Functions --

(-> source-parinfer-run (pathname string) json-object)
(defun source-parinfer-run (library text)
  "Return parinfer's paren-mode answer for TEXT, loading LIBRARY when needed."
  (source-parinfer--load library)
  (json-decode
   (cffi:foreign-funcall "run_parinfer"
                         :string (json-encode
                                  (json-object "mode" "paren"
                                               "text" text
                                               "options" (apply #'json-object
                                                                *source-parinfer-options*)))
                         :string)))

(-> source-parinfer-library-name () string)
(defun source-parinfer-library-name ()
  "Return the file name of the parinfer library on this host."
  #+darwin "libparinfer_rust.dylib"
  #+win32 "parinfer_rust.dll"
  #-(or darwin win32) "libparinfer_rust.so")


;;;; -- Private Functions --

(-> source-parinfer--load (pathname) null)
(defun source-parinfer--load (library)
  "Load parinfer LIBRARY into this image once."
  (unless (equal *source-parinfer-loaded-library* library)
    (unless (probe-file library)
      (error 'source-parinfer-error
             :library library
             :message (format nil "The parinfer library ~A is missing; run ./script/check, which builds it."
                              library)))
    (handler-case (cffi:load-foreign-library library)
      (error (condition)
        (error 'source-parinfer-error
               :library library
               :message (format nil "The parinfer library ~A cannot be loaded: ~A"
                                library condition))))
    (setf *source-parinfer-loaded-library* library))
  nil)


;;;; -- Conditions --

(define-condition source-parinfer-error (autolith-error)
  ((library
    :initarg       :library
    :reader        source-parinfer-error-library
    :documentation "The library that could not be used."))
  (:documentation "The private parinfer library is missing or cannot be loaded."))
