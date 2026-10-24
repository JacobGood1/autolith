;;;; -- Clean Compile Warnings --

;;; Compile and load every Autolith system from scratch into a private FASL
;;; directory and record each warning the compiler or loader would print.
;;; Warnings SBCL already muffles by policy, such as uninteresting
;;; redefinitions, are not recorded, nor are those of dependencies or ASDF's
;;; per-file summaries of them. The parent check reports what this records.
;;;
;;; Usage: compile-warnings.lisp SOURCE-ROOT FASL-DIRECTORY RESULT-FILE

(require :asdf)

(defparameter *compile-warnings-systems*
  '("autolith" "autolith/checks" "autolith/tests" "autolith/release-server"
    "autolith/structural" "autolith/structural/tests" "autolith/debug"
    "autolith/debug/tests")
  "Every system autolith.asd defines.")

(defun compile-warnings--source-path (source-root fasl-directory)
  "Return the repository-relative source path of the file being compiled or loaded, or NIL."
  (let ((compiled *compile-file-truename*)
        (loaded   *load-truename*))
    (cond
      ((and compiled (uiop:subpathp compiled source-root))
       (uiop:unix-namestring (uiop:enough-pathname compiled source-root)))
      ((and loaded (uiop:subpathp loaded fasl-directory))
       (let ((relative (uiop:enough-pathname loaded fasl-directory)))
         (uiop:unix-namestring (make-pathname :type "lisp" :defaults relative))))
      (t
       nil))))

(defun compile-warnings--line (source-root path)
  "Return the source line of the form the compiler is processing in PATH, or NIL."
  (let ((position (ignore-errors
                   (let ((context (sb-c::find-error-context nil)))
                     (and context (sb-c::compiler-error-context-file-position context))))))
    (and position
         path
         (with-open-file (stream (merge-pathnames path source-root)
                                 :external-format ':utf-8)
           (let ((text (make-string position)))
             (read-sequence text stream)
             (1+ (count #\Newline text)))))))

(defun compile-warnings--interesting-p (condition)
  "Return true when CONDITION would be printed and is not ASDF's summary of others."
  (not (or (typep condition sb-ext:*muffled-warnings*)
           (typep condition 'uiop:compile-warned-warning)
           (typep condition 'uiop:compile-failed-warning))))

(let* ((arguments      (uiop:command-line-arguments))
       (source-root    (uiop:ensure-directory-pathname (first arguments)))
       (fasl-directory (uiop:ensure-directory-pathname (second arguments)))
       (result-file    (pathname (third arguments)))
       (warnings       nil))
  (load (merge-pathnames ".qlot/setup.lisp" source-root))
  (asdf:initialize-output-translations
   `(:output-translations (,(uiop:native-namestring source-root)
                           ,(uiop:native-namestring fasl-directory))
                          :inherit-configuration))
  (asdf:load-asd (merge-pathnames "autolith.asd" source-root))
  (handler-bind ((warning
                   (lambda (condition)
                     (when (compile-warnings--interesting-p condition)
                       (let ((path (compile-warnings--source-path source-root fasl-directory)))
                         ;; A warning without a file of ours belongs to a
                         ;; dependency, unless no file is being processed at
                         ;; all, as for undefined functions reported at the end.
                         (unless (and (null path) (or *compile-file-truename* *load-truename*))
                           (push (list :path    path
                                       :line    (compile-warnings--line source-root path)
                                       :type    (string-downcase (type-of condition))
                                       :message (substitute #\Space #\Newline
                                                            (princ-to-string condition)))
                                 warnings)))))))
    (let ((asdf:*compile-file-warnings-behaviour* ':warn)
          (asdf:*compile-file-failure-behaviour*  ':warn))
      (dolist (system *compile-warnings-systems*)
        (when (asdf:find-system system nil)
          (asdf:load-system system)))))
  (with-open-file (stream result-file :direction ':output :if-exists ':supersede
                                      :external-format ':utf-8)
    (with-standard-io-syntax
      (let ((*print-readably* nil))
        (prin1 (reverse warnings) stream))))
  (uiop:quit 0))
