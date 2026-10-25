(in-package #:autolith)

;;;; -- Mallet --

;;; Mallet (https://github.com/fukamachi/mallet) reports mistakes the
;;; repository's own rules do not look for. Only the Mallet rules compatible
;;; with Autolith run, and their findings are reported, never repaired.
;;;
;;; Rules left out because they contradict Autolith's design or AGENTS.md:
;;; one-package-per-file (one project package spans every file); no-eval,
;;; runtime-intern, and runtime-unintern (a live, self-modifying image
;;; evaluates code and creates symbols at run time); double-colon-access
;;; (platform adapters reach SBCL and ASDF internals); needless-let*
;;; (AGENTS.md aligns independent LET* bindings); asdf-component-strings and
;;; asdf-redundant-package-prefix (autolith.asd names components with
;;; uninterned symbols and qualifies ASDF's); no-allow-other-keys (protocol
;;; methods accept their generic function's keys); missing-else,
;;; missing-otherwise, progn-in-conditional, bare-float-literal, and
;;; constant-naming (taste, or constants, which are banned outright); and
;;; line-length, function-length, cyclomatic-complexity, and comment-ratio
;;; (limits AGENTS.md does not set).
;;;
;;; Rules left out because another check owns them: trailing-whitespace,
;;; no-tabs, missing-final-newline, and missing-docstring belong to the
;;; repository's rules, and unused-variables and unused-local-functions to the
;;; compiler warnings, which read conditionalized code correctly.
;;; special-variable-naming misreads macro lambda lists, and the Coalton rules
;;; have nothing to inspect.
;;;
;;; Code that breaks a rule on purpose says why where it does, with a comment
;;; such as "; mallet:suppress no-ignore-errors -- best-effort cleanup".

(defparameter *source-mallet-rules*
  '(:wrong-otherwise
    :mixed-optional-and-key
    :asdf-if-feature-keyword
    :asdf-secondary-system-name
    :asdf-operate-in-perform
    :asdf-reader-conditional
    :no-package-use
    :defpackage-interned-symbol
    :unused-imported-symbols
    :unused-local-nicknames
    :unused-loop-variables
    :redundant-progn
    :closing-paren-on-own-line
    :consecutive-blank-lines
    :missing-package-docstring
    :missing-variable-docstring
    :missing-struct-docstring
    :stale-suppression)
  "The Mallet rules every tracked Lisp file follows.")

(defparameter *source-mallet-project-rules*
  '(:no-ignore-errors
    :error-without-custom-condition)
  "The Mallet rules project source follows as well.

Standalone scripts, the recovery image, and tests cannot rely on Autolith's
conditions, so only files below *SOURCE-MALLET-PROJECT-DIRECTORIES* follow
these.")

(defparameter *source-mallet-project-directories* '("src/" "checks/" "server/")
  "Directories whose files follow *SOURCE-MALLET-PROJECT-RULES*.")

(defclass source-mallet-rule (source-rule)
  ()
  (:default-initargs
   :name        "mallet"
   :description "Mallet's rules compatible with Autolith, each finding named after its Mallet rule.")
  (:documentation "Mallet's linter, run over each Lisp file."))


;;;; -- Methods --

(defmethod source-rule-findings ((rule source-mallet-rule) (file source-file))
  "Report the violations Mallet's selected rules find in FILE."
  (declare (ignore rule))
  (mapcar (lambda (violation)
            (source-mallet--finding file violation))
          (source-mallet-violations file)))


;;;; -- Public Functions --

(-> source-mallet-configuration (string) mallet/config:config)
(defun source-mallet-configuration (path)
  "Return the Mallet configuration for the file at repository-relative PATH."
  (mallet/config:make-config
   :rules (mapcar #'mallet/rules:make-rule
                  (append *source-mallet-rules*
                          (and (source-mallet--project-path-p path)
                               *source-mallet-project-rules*)))))

(-> source-mallet-violations (source-file) list)
(defun source-mallet-violations (file)
  "Return the violations Mallet reports in FILE under the rules its path follows.

Mallet reads files, so FILE's text is linted from a file of the same name in a
private temporary directory; some rules compare definitions with that name."
  (let ((directory (platform-make-temporary-directory *platform* (uiop:temporary-directory)
                                                      "autolith-mallet-")))
    (unwind-protect
         (let ((pathname (merge-pathnames (file-namestring (source-file-path file)) directory)))
           (with-open-file (stream pathname :direction ':output :external-format ':utf-8)
             (write-string (source-file-text file) stream))
           (mallet/engine:lint-file pathname
                                    :config (source-mallet-configuration (source-file-path file))))
      (platform-delete-directory-tree *platform* directory
                                      :validate t :if-does-not-exist ':ignore))))


;;;; -- Private Functions --

(-> source-mallet--project-path-p (string) boolean)
(defun source-mallet--project-path-p (path)
  "Return true when repository-relative PATH is project source."
  (and (some (lambda (directory)
               (uiop:string-prefix-p directory path))
             *source-mallet-project-directories*)
       t))

(-> source-mallet--finding (source-file mallet/violation:violation) source-finding)
(defun source-mallet--finding (file violation)
  "Return VIOLATION, which Mallet reported in FILE, as a finding named after its rule."
  (make-instance 'source-finding
                 :rule    (format nil "mallet/~(~A~)" (mallet/violation:violation-rule violation))
                 :path    (source-file-path file)
                 :line    (max 1 (or (mallet/violation:violation-line violation) 1))
                 :column  (max 0 (or (mallet/violation:violation-column violation) 0))
                 :message (mallet/violation:violation-message violation)))
