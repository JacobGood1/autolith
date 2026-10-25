(in-package #:autolith)

;;;; -- Compiler Warnings --

;;; Warnings nobody silenced on purpose are failures. A clean compile of every
;;; system runs in its own process, script/compile-warnings.lisp, with a
;;; private FASL directory, so the cached FASLs every other process uses never
;;; hide a warning. Each warning it records becomes a finding.

(defclass source-warning-run ()
  ((process
    :initarg       :process
    :reader        source-warning-run-process
    :documentation "The compiling process, as UIOP:LAUNCH-PROGRAM returned it.")
   (result
    :initarg       :result
    :reader        source-warning-run-result
    :type          pathname
    :documentation "The file the compiling process writes its warnings to.")
   (log
    :initarg       :log
    :reader        source-warning-run-log
    :type          pathname
    :documentation "The file holding the compiling process's output."))
  (:documentation "A clean compile running in its own process."))


;;;; -- Public Functions --

(-> source-warning-run-start
    (pathname &key (:command list) (:directory pathname))
    source-warning-run)
(defun source-warning-run-start (root &key command directory)
  "Start compiling ROOT's systems from scratch below DIRECTORY with COMMAND, a Lisp command line.

COMMAND runs script/compile-warnings.lisp as a script, such as the runtime
command followed by --script. The compile writes its FASLs, result, and log
below DIRECTORY."
  (let ((fasls  (merge-pathnames "fasl/" directory))
        (result (merge-pathnames "warnings.sexp" directory))
        (log    (merge-pathnames "compile.log" directory)))
    (ensure-directories-exist fasls)
    (make-instance 'source-warning-run
                   :result  result
                   :log     log
                   :process (uiop:launch-program
                             (append command
                                     (list (uiop:native-namestring
                                            (merge-pathnames "script/compile-warnings.lisp" root))
                                           (uiop:native-namestring root)
                                           (uiop:native-namestring fasls)
                                           (uiop:native-namestring result)))
                             :output           log
                             :error-output     ':output
                             :if-output-exists ':supersede))))

(-> source-warning-run-finish (source-warning-run) list)
(defun source-warning-run-finish (run)
  "Wait for RUN and return a finding for each warning it recorded.

Signal SOURCE-WARNING-RUN-FAILED when the compile did not finish."
  (let ((status (uiop:wait-process (source-warning-run-process run))))
    (unless (and (eql status 0) (probe-file (source-warning-run-result run)))
      (error 'source-warning-run-failed
             :log     (source-warning-run-log run)
             :message (format nil "The clean compile failed with status ~A.~%~A"
                              status (source-warning--output-tail (source-warning-run-log run)))))
    (source-warning-findings
     (with-open-file (stream (source-warning-run-result run) :external-format ':utf-8)
       (let ((*read-eval* nil)
             (*package*   (find-package '#:autolith)))
         (read stream))))))

(-> source-warning-findings (list) list)
(defun source-warning-findings (records)
  "Return a finding for each distinct warning record, a plist of :PATH, :LINE, :TYPE, and :MESSAGE.

A form the compiler processes more than once reports the same warning at the
same line each time; it is one finding."
  (remove-duplicates
   (mapcar (lambda (record)
             (make-instance 'source-finding
                            :rule    "warning"
                            :path    (or (getf record :path) "(end of compilation)")
                            :line    (or (getf record :line) 1)
                            :message (format nil "~A: ~A"
                                             (getf record :type)
                                             (source-warning--squeeze (getf record :message)))))
           records)
   :test     (lambda (finding other)
               (and (equal (source-finding-path finding) (source-finding-path other))
                    (eql (source-finding-line finding) (source-finding-line other))
                    (equal (source-finding-message finding) (source-finding-message other))))
   :from-end t))


;;;; -- Private Functions --

(-> source-warning--output-tail (pathname) string)
(defun source-warning--output-tail (pathname)
  "Read at most the last 16,384 characters of PATHNAME into the failure report."
  (handler-case
      (with-open-file (stream pathname :external-format ':utf-8)
        (let ((buffer (make-string 8192))
              (tail   ""))
          (loop for count = (read-sequence buffer stream)
                while (plusp count)
                do (let ((text (concatenate 'string tail (subseq buffer 0 count))))
                     (setf tail (subseq text (max 0 (- (length text) 16384))))))
          tail))
    (file-error (condition)
      (format nil "Compiler output unavailable: ~A" condition))))

(-> source-warning--squeeze (string) string)
(defun source-warning--squeeze (text)
  "Return TEXT with each run of whitespace written as one space."
  (with-output-to-string (stream)
    (let ((blank-p nil))
      (loop for character across (string-trim '(#\Space #\Tab #\Newline) text)
            do (if (source-text--blank-character-p character)
                   (unless blank-p
                     (write-char #\Space stream)
                     (setf blank-p t))
                   (progn
                     (write-char character stream)
                     (setf blank-p nil)))))))


;;;; -- Conditions --

(define-condition source-warning-run-failed (autolith-error)
  ((log
    :initarg       :log
    :reader        source-warning-run-failed-log
    :type          pathname
    :documentation "The compiling process's output."))
  (:documentation "The clean compile did not finish."))
