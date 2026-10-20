(in-package #:autolith)

;;;; -- Knowledge Log --

;;; Structured knowledge is an append-only log beside the memory log. Older
;;; Autolith generations never read it, so a rollback keeps every prose memory
;;; readable. Records define schemas, record evidence, assert supports for
;;; claims, and retract supports. Reading folds the log into a KNOWLEDGE-BASE
;;; with indexes; reads are cached by the log file's identity, because every
;;; publication replaces the file.

(defparameter *knowledge-format-version* 1
  "The readable knowledge log format version.")

(defparameter *knowledge-excerpt-limit* 400
  "The maximum characters of evidence text retained in the log.")

(defparameter *knowledge-reason-limit* 400
  "The maximum characters of a retraction reason.")

(deftype knowledge-basis ()
  "Why a support is believed."
  '(member :observation :user-assertion :assumption :model-proposal))

(defvar *knowledge-lock* (make-recursive-lock "Autolith structured knowledge")
  "Serialize same-process knowledge transactions.")

(defvar *knowledge-base-cache* (make-hash-table :test #'equal :synchronized t)
  "Folded knowledge bases by log namestring, each with the file status it reflects.")

(defclass knowledge-base ()
  ((revision
    :initform 0
    :accessor knowledge-base-revision
    :type (integer 0)
    :documentation "How many records the log held; each record is one graph revision.")
   (schemas
    :initform (make-hash-table :test #'equal)
    :reader knowledge-base-schemas
    :type hash-table
    :documentation "(SCOPE WORKSPACE PREDICATE) to (SCHEMA VERSION).")
   (evidence
    :initform (make-hash-table :test #'equal)
    :reader knowledge-base-evidence
    :type hash-table
    :documentation "Evidence records by identifier.")
   (supports
    :initform (make-hash-table :test #'equal)
    :reader knowledge-base-supports
    :type hash-table
    :documentation "Support records by identifier, retracted ones included.")
   (claims
    :initform (make-hash-table :test #'equal)
    :reader knowledge-base-claims
    :type hash-table
    :documentation "Claim plists by identifier: proposition, scope, workspace, supports.")
   (predicate-index
    :initform (make-hash-table :test #'equal)
    :reader knowledge-base-predicate-index
    :type hash-table
    :documentation "Claim identifiers by predicate name.")
   (term-index
    :initform (make-hash-table :test #'equal)
    :reader knowledge-base-term-index
    :type hash-table
    :documentation "Claim identifiers by each string argument, for entity lookup.")
   (evidence-index
    :initform (make-hash-table :test #'equal)
    :reader knowledge-base-evidence-index
    :type hash-table
    :documentation "Support identifiers by the evidence they rest on.")
   (rules
    :initform (make-hash-table :test #'equal)
    :reader knowledge-base-rules
    :type hash-table
    :documentation "Rule entries by (SCOPE WORKSPACE IDENTIFIER)."))
  (:documentation "The folded state of the knowledge log, with lookup indexes."))


;;;; -- Record Validation --

(-> knowledge--scope-valid-p (t t) boolean)
(defun knowledge--scope-valid-p (scope workspace)
  "Return true when SCOPE and WORKSPACE describe a global or workspace owner."
  (and (typep scope 'memory-scope)
       (if (eq scope ':workspace)
           (non-empty-string-p workspace)
           (null workspace))
       t))

(-> knowledge-evidence-source-p (t) boolean)
(defun knowledge-evidence-source-p (source)
  "Return true when SOURCE is a portable evidence source description."
  (and (proper-list-p source)
       (evenp (length (rest source)))
       (let ((properties (rest source)))
         (flet ((optional-text-p (key limit)
                  (let ((value (getf properties key)))
                    (or (null value) (and (stringp value) (<= (length value) limit))))))
           (case (first source)
             (:file
              (and (non-empty-string-p (getf properties :path))
                   (member (getf properties :relative) '(nil t))
                   (optional-text-p :quote *memory-quote-limit*)))
             (:url
              (and (non-empty-string-p (getf properties :url))
                   (<= (length (getf properties :url)) *memory-source-url-limit*)
                   (optional-text-p :quote *memory-quote-limit*)))
             (:user
              (and (memory-source-quote-p (getf properties :quote))
                   (optional-text-p :conversation 200)))
             (:tool
              (and (non-empty-string-p (getf properties :name))
                   (optional-text-p :conversation 200)
                   (optional-text-p :call 200)))
             (:memory
              (and (non-empty-string-p (getf properties :id))
                   (memory-source-digest-p (getf properties :digest))))
             ((:mission :skill)
              (and (non-empty-string-p (getf properties :id))
                   (optional-text-p :detail 200)))
             (otherwise
              nil))))
       t))

(-> knowledge--evidence-record-p (list) boolean)
(defun knowledge--evidence-record-p (record)
  "Return true when RECORD is a complete evidence record."
  (let ((properties (rest record)))
    (and (non-empty-string-p (getf properties :id))
         (knowledge-evidence-source-p (getf properties :source))
         (or (null (getf properties :workspace))
             (non-empty-string-p (getf properties :workspace)))
         (or (null (getf properties :digest))
             (memory-source-digest-p (getf properties :digest)))
         (typep (getf properties :observed-at) 'timestamp)
         (or (null (getf properties :valid-until))
             (typep (getf properties :valid-until) 'timestamp))
         (or (null (getf properties :excerpt))
             (and (stringp (getf properties :excerpt))
                  (<= (length (getf properties :excerpt)) *knowledge-excerpt-limit*)))
         (non-empty-string-p (getf properties :adapter))
         t)))

(-> knowledge--support-record-p (list) boolean)
(defun knowledge--support-record-p (record)
  "Return true when RECORD is a complete claim assertion record."
  (let ((properties (rest record)))
    (and (non-empty-string-p (getf properties :support))
         (non-empty-string-p (getf properties :claim))
         (knowledge--scope-valid-p (getf properties :scope) (getf properties :workspace))
         (proper-list-p (getf properties :proposition))
         (or (null (getf properties :evidence))
             (non-empty-string-p (getf properties :evidence)))
         (typep (getf properties :basis) 'knowledge-basis)
         (or (getf properties :evidence)
             (eq (getf properties :basis) ':assumption))
         (or (null (getf properties :valid-from))
             (typep (getf properties :valid-from) 'timestamp))
         (or (null (getf properties :valid-until))
             (typep (getf properties :valid-until) 'timestamp))
         (typep (getf properties :time) 'timestamp)
         t)))

(-> knowledge--record-valid-p (t) boolean)
(defun knowledge--record-valid-p (record)
  "Return true when RECORD is well formed. Unknown kinds are left to the reducer."
  (and (proper-list-p record)
       (keywordp (first record))
       (evenp (length (rest record)))
       (case (first record)
         (:schema-defined
          (let ((properties (rest record)))
            (and (knowledge--scope-valid-p (getf properties :scope)
                                           (getf properties :workspace))
                 (knowledge-schema-p (getf properties :schema))
                 (typep (getf properties :time) 'timestamp))))
         (:evidence
          (knowledge--evidence-record-p record))
         (:claim-asserted
          (knowledge--support-record-p record))
         (:support-retracted
          (let ((properties (rest record)))
            (and (non-empty-string-p (getf properties :support))
                 (typep (getf properties :time) 'timestamp)
                 (or (null (getf properties :reason))
                     (and (stringp (getf properties :reason))
                          (<= (length (getf properties :reason))
                              *knowledge-reason-limit*))))))
         (otherwise
          t))
       t))


;;;; -- Folding --

(-> knowledge--index-add (hash-table t string) null)
(defun knowledge--index-add (index key identifier)
  "Add IDENTIFIER under KEY in INDEX once."
  (pushnew identifier (gethash key index) :test #'string=)
  nil)

(-> knowledge--reduce (pathname knowledge-base list) knowledge-base)
(defun knowledge--reduce (pathname base record)
  "Fold one validated RECORD into fresh BASE.

Records of kinds this generation does not know are skipped, so a log written
by a newer generation stays readable after a rollback."
  (flet ((fail (message)
           (error 'knowledge-error
                  :code ':corrupt
                  :subject (namestring pathname)
                  :message message)))
    (let ((properties (rest record)))
      (case (first record)
        (:schema-defined
         (let* ((schema (getf properties :schema))
                (key (list (getf properties :scope)
                           (getf properties :workspace)
                           (knowledge-schema-predicate schema)))
                (previous (gethash key (knowledge-base-schemas base))))
           (setf (gethash key (knowledge-base-schemas base))
                 (list schema (if previous (1+ (second previous)) 1)))))
        (:evidence
         (setf (gethash (getf properties :id) (knowledge-base-evidence base))
               (copy-list properties)))
        (:claim-asserted
         (let ((claim (getf properties :claim))
               (support (getf properties :support))
               (evidence (getf properties :evidence)))
           (when (and evidence
                      (null (gethash evidence (knowledge-base-evidence base))))
             (fail (format nil "Support ~A cites unknown evidence ~A." support evidence)))
           (setf (gethash support (knowledge-base-supports base))
                 (copy-list properties))
           (let ((entry (gethash claim (knowledge-base-claims base))))
             (if entry
                 (pushnew support (getf entry :supports) :test #'string=)
                 (let ((proposition (getf properties :proposition)))
                   (setf (gethash claim (knowledge-base-claims base))
                         (list :id claim
                               :proposition proposition
                               :scope (getf properties :scope)
                               :workspace (getf properties :workspace)
                               :supports (list support)))
                   (knowledge--index-add (knowledge-base-predicate-index base)
                                         (knowledge-proposition-predicate proposition)
                                         claim)
                   (dolist (term (knowledge-proposition-arguments proposition))
                     (when (stringp term)
                       (knowledge--index-add (knowledge-base-term-index base)
                                             term claim))))))
           (when evidence
             (knowledge--index-add (knowledge-base-evidence-index base) evidence support))))
        (:support-retracted
         (let ((support (gethash (getf properties :support)
                                 (knowledge-base-supports base))))
           (unless support
             (fail (format nil "Retraction names unknown support ~A."
                           (getf properties :support))))
           (setf (getf support :retracted-at) (getf properties :time)
                 (getf support :retraction-reason) (getf properties :reason)
                 (gethash (getf properties :support) (knowledge-base-supports base))
                 support)))
        (otherwise
         (knowledge--reduce-extension base record))))
    (incf (knowledge-base-revision base))
    base))

(-> knowledge--reduce-extension (knowledge-base list) null)
(defgeneric knowledge--reduce-extension (base record)
  (:documentation "Fold RECORD of a kind defined outside the core log into BASE.")
  (:method ((base knowledge-base) record)
    "Skip a record kind this generation does not know."
    (declare (ignore base record))
    nil))

(-> knowledge--store (configuration) sexp-store:log-store)
(defun knowledge--store (configuration)
  "Describe the knowledge log schema, reducer, and paths."
  (let ((pathname (configuration-knowledge-path configuration)))
    (make-instance
     'sexp-store:log-store
     :pathname pathname
     :lock-pathname (readable-state-lock-pathname pathname "knowledge.lock")
     :header (list :knowledge :version *knowledge-format-version*)
     :header-validator
     (lambda (form)
       (sexp-store:record-check form :tag ':knowledge
                               :versions (list *knowledge-format-version*)))
     :validator #'knowledge--record-valid-p
     :initial-state (lambda () (make-instance 'knowledge-base))
     :reducer (lambda (base record)
                (knowledge--reduce pathname base record)))))

(-> knowledge--file-key (pathname) t)
(defun knowledge--file-key (pathname)
  "Return a value that changes whenever the log at PATHNAME is republished."
  (let ((status (platform-path-status *platform* pathname :follow-links-p t)))
    (and status
         (list (platform-file-status-identity status)
               (platform-file-status-size status)
               (platform-file-status-modification-time status)
               (platform-file-status-change-time status)))))

(-> knowledge--translate-store-errors (function) t)
(defun knowledge--translate-store-errors (function)
  "Call FUNCTION, reporting storage failures as KNOWLEDGE-ERROR."
  (handler-case (funcall function)
    (sexp-store:store-error (cause)
      (error 'knowledge-error
             :code ':storage
             :subject (namestring (sexp-store:store-error-pathname cause))
             :message (sexp-store:store-error-message cause)))))


;;;; -- Reading and Transactions --

(-> knowledge-base-read (configuration) knowledge-base)
(defun knowledge-base-read (configuration)
  "Return CONFIGURATION's folded knowledge base, reusing an unchanged read.

The returned base is shared; callers must not modify it."
  (let* ((pathname (configuration-knowledge-path configuration))
         (name (namestring pathname))
         (key (knowledge--file-key pathname))
         (cached (gethash name *knowledge-base-cache*)))
    (if (and cached key (equal (first cached) key))
        (rest cached)
        (let ((base (knowledge--translate-store-errors
                     (lambda ()
                       (with-recursive-lock-held (*knowledge-lock*)
                         (sexp-store:store-read (knowledge--store configuration)))))))
          (when key
            (setf (gethash name *knowledge-base-cache*) (cons key base)))
          base))))

(-> knowledge-transact (configuration function) t)
(defun knowledge-transact (configuration update)
  "Apply UPDATE to a fresh knowledge base and publish its records atomically.

UPDATE receives a private base and returns RECORDS, RESULT, and WRITE-P as for
SEXP-STORE:STORE-TRANSACT. Return RESULT."
  (with-recursive-lock-held (*knowledge-lock*)
    (values
     (knowledge--translate-store-errors
      (lambda ()
        (sexp-store:store-transact (knowledge--store configuration) update))))))


;;;; -- Lookup --

(-> knowledge-base-schema (knowledge-base string &key (:workspace (option string)))
    (values (option list) (option integer)))
(defun knowledge-base-schema (base predicate &key workspace)
  "Return PREDICATE's schema visible from WORKSPACE and its version, or NIL.

A workspace definition shadows a global one."
  (let ((entry (or (and workspace
                        (gethash (list ':workspace workspace predicate)
                                 (knowledge-base-schemas base)))
                   (gethash (list ':global nil predicate)
                            (knowledge-base-schemas base)))))
    (values (first entry) (second entry))))

(-> knowledge-visible-p (memory-scope (option string) (option string)) boolean)
(defun knowledge-visible-p (scope workspace viewer)
  "Return true when knowledge owned by SCOPE and WORKSPACE applies in workspace VIEWER."
  (and (or (eq scope ':global) (equal workspace viewer)) t))

(-> knowledge-base-claim (knowledge-base string) (option list))
(defun knowledge-base-claim (base identifier)
  "Return claim IDENTIFIER's plist, or NIL."
  (values (gethash identifier (knowledge-base-claims base))))

(-> knowledge-base-support (knowledge-base string) (option list))
(defun knowledge-base-support (base identifier)
  "Return support IDENTIFIER's plist, retracted or not, or NIL."
  (values (gethash identifier (knowledge-base-supports base))))

(-> knowledge-base-evidence-record (knowledge-base string) (option list))
(defun knowledge-base-evidence-record (base identifier)
  "Return evidence IDENTIFIER's plist, or NIL."
  (values (gethash identifier (knowledge-base-evidence base))))

(-> knowledge-base-claims-with-predicate (knowledge-base string) list)
(defun knowledge-base-claims-with-predicate (base predicate)
  "Return the identifiers of claims about PREDICATE."
  (values (gethash predicate (knowledge-base-predicate-index base))))

(-> knowledge-base-claims-mentioning (knowledge-base string) list)
(defun knowledge-base-claims-mentioning (base term)
  "Return the identifiers of claims with string argument TERM, such as an entity."
  (values (gethash term (knowledge-base-term-index base))))


;;;; -- Writing --

(-> knowledge--workspace (configuration) string)
(defun knowledge--workspace (configuration)
  "Return CONFIGURATION's workspace identity, as memories record it."
  (namestring (config :working-directory configuration)))

(-> knowledge--owner (configuration memory-scope) (option string))
(defun knowledge--owner (configuration scope)
  "Return the workspace owning knowledge in SCOPE, or NIL for global knowledge."
  (and (eq scope ':workspace) (knowledge--workspace configuration)))

(-> knowledge-define-schema
    (configuration list &key (:scope memory-scope) (:source-conversation (option string)))
    (values list integer))
(defun knowledge-define-schema (configuration schema &key (scope ':workspace) source-conversation)
  "Define or redefine predicate SCHEMA in SCOPE and return it and its version.

Redefining an unchanged schema writes nothing. A redefinition may change the
key, template, and description; changing arguments while claims use the
predicate is refused, because those claims would no longer fit."
  (unless (knowledge-schema-p schema)
    (knowledge--fail ':invalid-schema schema "The schema is malformed."))
  (let ((workspace (knowledge--owner configuration scope))
        (predicate (knowledge-schema-predicate schema)))
    (values-list
     (knowledge-transact
     configuration
     (lambda (base)
       (destructuring-bind (&optional existing version)
           (gethash (list scope workspace predicate) (knowledge-base-schemas base))
         (cond
           ((equal existing schema)
            (values nil (list existing version) nil))
           ((and existing
                 (not (equal (knowledge-schema-arguments existing)
                             (knowledge-schema-arguments schema)))
                 (some (lambda (claim)
                         (let ((entry (knowledge-base-claim base claim)))
                           (and (eq (getf entry :scope) scope)
                                (equal (getf entry :workspace) workspace))))
                       (knowledge-base-claims-with-predicate base predicate)))
            (knowledge--fail ':schema-in-use predicate
                             "Claims already use ~A; keep its arguments, or define a new predicate."
                             predicate))
           (t
            (values (list (list :schema-defined
                                :version *knowledge-format-version*
                                :scope scope
                                :workspace workspace
                                :schema schema
                                :time (get-universal-time)
                                :source-conversation source-conversation))
                    (list schema (if version (1+ version) 1))
                    t)))))))))

(-> knowledge-make-evidence
    (&key (:source list) (:workspace (option string)) (:digest (option string))
          (:observed-at timestamp) (:valid-until (option timestamp))
          (:excerpt (option string)) (:adapter string))
    list)
(defun knowledge-make-evidence
    (&key source workspace digest observed-at valid-until excerpt adapter)
  "Return a validated evidence record for one observation.

The identifier depends on the source, digest, adapter, and workspace, so the
same observation recorded twice is one piece of evidence."
  (let ((record (list :evidence
                      :version *knowledge-format-version*
                      :id (knowledge-digest-identifier
                           #\e (list source digest adapter workspace))
                      :source source
                      :workspace workspace
                      :digest digest
                      :observed-at observed-at
                      :valid-until valid-until
                      :excerpt (and excerpt
                                    (subseq excerpt 0 (min (length excerpt)
                                                           *knowledge-excerpt-limit*)))
                      :adapter adapter)))
    (unless (knowledge--evidence-record-p record)
      (knowledge--fail ':invalid-evidence source "The evidence description is malformed."))
    record))

(-> knowledge-assert
    (configuration list
     &key (:scope memory-scope) (:evidence (option list)) (:basis knowledge-basis)
          (:valid-from (option timestamp)) (:valid-until (option timestamp))
          (:source-conversation (option string)))
    (values string string))
(defun knowledge-assert
    (configuration proposition
     &key (scope ':workspace) evidence basis valid-from valid-until source-conversation)
  "Support the claim PROPOSITION makes in SCOPE and return the claim and support IDs.

PROPOSITION must fit a schema visible from the workspace. EVIDENCE is a record
from KNOWLEDGE-MAKE-EVIDENCE, or NIL for an assumption. Asserting the same
support again writes nothing and returns the existing one."
  (let* ((workspace (knowledge--owner configuration scope))
         (viewer (knowledge--workspace configuration))
         (claim (knowledge-claim-identifier proposition :scope scope :workspace workspace)))
    (unless (typep basis 'knowledge-basis)
      (knowledge--fail ':invalid-claim basis
                       "Basis must be observation, user-assertion, assumption, or model-proposal."))
    (when (and (null evidence) (not (eq basis ':assumption)))
      (knowledge--fail ':invalid-claim basis
                       "Only an assumption may be asserted without evidence."))
    (when (and valid-from valid-until (> valid-from valid-until))
      (knowledge--fail ':invalid-claim valid-until
                       "A validity interval cannot end before it starts."))
    (values-list
     (knowledge-transact
     configuration
     (lambda (base)
       (let ((schema (knowledge-base-schema base (knowledge-proposition-predicate proposition)
                                            :workspace viewer)))
         (unless schema
           (knowledge--fail ':unknown-predicate (knowledge-proposition-predicate proposition)
                            "Define predicate ~A before asserting claims about it."
                            (knowledge-proposition-predicate proposition)))
         (when (and (eq scope ':global)
                    (null (knowledge-base-schema base (knowledge-proposition-predicate proposition))))
           (knowledge--fail ':unknown-predicate (knowledge-proposition-predicate proposition)
                            "A global claim needs a global definition of ~A."
                            (knowledge-proposition-predicate proposition)))
         (unless (knowledge-proposition-valid-p proposition schema)
           (knowledge--fail ':invalid-claim (knowledge-proposition-predicate proposition)
                            "The claim does not fit the schema of ~A."
                            (knowledge-proposition-predicate proposition)))
         (let* ((evidence-identifier (and evidence (getf (rest evidence) :id)))
                (existing
                  (find-if (lambda (support-identifier)
                             (let ((support (knowledge-base-support base support-identifier)))
                               (and (null (getf support :retracted-at))
                                    (equal (getf support :evidence) evidence-identifier)
                                    (eq (getf support :basis) basis)
                                    (eql (getf support :valid-from) valid-from)
                                    (eql (getf support :valid-until) valid-until))))
                           (getf (knowledge-base-claim base claim) :supports))))
           (if existing
               (values nil (list claim existing) nil)
               (let ((support (make-identifier)))
                 (values
                  (append
                   (when (and evidence
                              (null (knowledge-base-evidence-record base evidence-identifier)))
                     (list evidence))
                   (list (list :claim-asserted
                               :version *knowledge-format-version*
                               :support support
                               :claim claim
                               :scope scope
                               :workspace workspace
                               :proposition proposition
                               :evidence evidence-identifier
                               :basis basis
                               :valid-from valid-from
                               :valid-until valid-until
                               :time (get-universal-time)
                               :source-conversation source-conversation)))
                  (list claim support)
                  t))))))))))

(-> knowledge-retract
    (configuration string &key (:reason (option string)) (:source-conversation (option string)))
    list)
(defun knowledge-retract (configuration support &key reason source-conversation)
  "Retract SUPPORT, or every active support of a claim when SUPPORT is a claim ID.

Return the retracted support identifiers. Retracted supports stay inspectable."
  (when (and reason (> (length reason) *knowledge-reason-limit*))
    (setf reason (subseq reason 0 *knowledge-reason-limit*)))
  (knowledge-transact
   configuration
   (lambda (base)
     (let* ((claim (knowledge-base-claim base support))
            (targets
              (remove-if (lambda (identifier)
                           (getf (knowledge-base-support base identifier) :retracted-at))
                         (cond
                           (claim
                            (getf claim :supports))
                           ((knowledge-base-support base support)
                            (list support))
                           (t
                            (knowledge--fail ':unknown-support support
                                             "No claim or support ~A exists." support)))))
            (time (get-universal-time)))
       (values (mapcar (lambda (identifier)
                         (list :support-retracted
                               :version *knowledge-format-version*
                               :support identifier
                               :reason reason
                               :time time
                               :source-conversation source-conversation))
                       targets)
               targets
               (and targets t))))))
