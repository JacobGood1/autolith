(in-package #:autolith)

;;;; -- Knowledge Evaluation --

;;; Evaluation turns the stored base into a VIEW for one workspace at one time.
;;; A stored claim is supported when one of its supports applies now. Admitted
;;; rules then derive conclusions from supported claims, in an order where
;;; every predicate a rule reads is complete before the rule runs; rules are
;;; acyclic, so one pass suffices. A derived conclusion carries a
;;; justification naming its rule version, premises, and bindings, and is
;;; supported exactly while its premises are. Nothing derived is stored: the
;;; view is a function of the log, the cited files, and the time, so a restart
;;; reproduces it and recomputation needs no model.

(defparameter *knowledge-match-limit* 4096
  "The most rule conclusions one evaluation derives before it is incomplete.")

(defparameter *knowledge-step-limit* 250000
  "The most candidate claims one evaluation examines before it is incomplete.")

(defparameter *knowledge-basis-set-limit* 4
  "The most distinct basis sets recorded for one claim.")

(defparameter *knowledge-error-limit* 32
  "The most rule evaluation errors one view retains.")

(defparameter *knowledge-basis-order*
  '(:observation :user-assertion :model-proposal :assumption)
  "The canonical order of bases within a basis set.")

(defclass knowledge-node ()
  ((identifier
    :initarg :identifier
    :reader knowledge-node-identifier
    :type string
    :documentation "The claim identifier.")
   (proposition
    :initarg :proposition
    :reader knowledge-node-proposition
    :type list
    :documentation "The canonical proposition the claim makes.")
   (scope
    :initarg :scope
    :reader knowledge-node-scope
    :type memory-scope
    :documentation "Whether the claim is global or belongs to a workspace.")
   (workspace
    :initarg :workspace
    :reader knowledge-node-workspace
    :type (option string)
    :documentation "The workspace owning a workspace claim.")
   (schema
    :initarg :schema
    :reader knowledge-node-schema
    :type (option list)
    :documentation "The schema of the claim's predicate visible in the view.")
   (supports
    :initform nil
    :accessor knowledge-node-supports
    :type list
    :documentation "(:ID :STATE :REASON :BASIS :EVIDENCE) for each stored support.")
   (justifications
    :initform nil
    :accessor knowledge-node-justifications
    :type list
    :documentation "(:ID :RULE :VERSION :PREMISES :BINDINGS) for each derivation.")
   (bases
    :initform nil
    :accessor knowledge-node-bases
    :type list
    :documentation "The minimal basis sets of the paths supporting the claim.")
   (conflicts
    :initform nil
    :accessor knowledge-node-conflicts
    :type list
    :documentation "Identifiers of supported claims this one contradicts."))
  (:documentation "One claim as evaluated in a view."))

(defclass knowledge-view ()
  ((base
    :initarg :base
    :reader knowledge-view-base
    :type knowledge-base
    :documentation "The folded log the view was evaluated from.")
   (workspace
    :initarg :workspace
    :reader knowledge-view-workspace
    :type (option string)
    :documentation "The workspace whose visible knowledge the view holds.")
   (now
    :initarg :now
    :reader knowledge-view-now
    :type timestamp
    :documentation "The time validity and expiry were evaluated at.")
   (nodes
    :initform (make-hash-table :test #'equal)
    :reader knowledge-view-nodes
    :type hash-table
    :documentation "Evaluated claims by identifier, stored and derived.")
   (supported
    :initform (make-hash-table :test #'equal)
    :reader knowledge-view-supported
    :type hash-table
    :documentation "Supported positive nodes by predicate.")
   (negated
    :initform (make-hash-table :test #'equal)
    :reader knowledge-view-negated
    :type hash-table
    :documentation "Supported negated nodes by predicate.")
   (argument-index
    :initform (make-hash-table :test #'equal)
    :reader knowledge-view-argument-index
    :type hash-table
    :documentation "Supported positive nodes by (PREDICATE POSITION STRING-TERM).")
   (incomplete
    :initform nil
    :accessor knowledge-view-incomplete
    :type list
    :documentation "Identifiers of rules whose evaluation hit a work limit.")
   (errors
    :initform nil
    :accessor knowledge-view-errors
    :type list
    :documentation "(:RULE :VERSION :MESSAGE) for derivations a computation refused.")
   (matches
    :initform 0
    :accessor knowledge-view-matches
    :type (integer 0)
    :documentation "Rule conclusions derived so far.")
   (steps
    :initform 0
    :accessor knowledge-view-steps
    :type (integer 0)
    :documentation "Candidate claims examined so far."))
  (:documentation "Knowledge visible from one workspace, evaluated at one time."))


;;;; -- Node Status --

(-> knowledge-node-supported-p (knowledge-node) boolean)
(defun knowledge-node-supported-p (node)
  "Return true when an applicable support or a derivation supports NODE."
  (and (or (knowledge-node-justifications node)
           (find ':current (knowledge-node-supports node)
                 :key (lambda (support) (getf support :state))))
       t))

(-> knowledge-node-derived-p (knowledge-node) boolean)
(defun knowledge-node-derived-p (node)
  "Return true when a rule derives NODE."
  (and (knowledge-node-justifications node) t))

(-> knowledge-node-negated-p (knowledge-node) boolean)
(defun knowledge-node-negated-p (node)
  "Return true when NODE claims an explicit negation."
  (knowledge-negation-p (knowledge-node-proposition node)))

(-> knowledge-node-status (knowledge-view knowledge-node) keyword)
(defun knowledge-node-status (view node)
  "Return NODE's logical status: :SUPPORTED, :CONFLICTED, :REFUTED, or :UNKNOWN.

A supported claim contradicted by another supported claim is conflicted; an
unsupported claim whose explicit negation is supported is refuted."
  (cond
    ((and (knowledge-node-supported-p node) (knowledge-node-conflicts node))
     ':conflicted)
    ((knowledge-node-supported-p node)
     ':supported)
    ((let ((negation (gethash (knowledge--negation-identifier node)
                              (knowledge-view-nodes view))))
       (and negation (knowledge-node-supported-p negation)))
     ':refuted)
    (t
     ':unknown)))

(-> knowledge-node-applicability (knowledge-node) keyword)
(defun knowledge-node-applicability (node)
  "Return whether NODE applies now, or the strongest reason it does not.

A supported claim is :CURRENT. Otherwise the reason is taken from its supports:
:STALE when evidence changed or disappeared, then :UNVERIFIED, :EXPIRED,
:PENDING, :RETRACTED, and :UNKNOWN when it has no supports at all."
  (if (knowledge-node-supported-p node)
      ':current
      (let ((states (mapcar (lambda (support) (getf support :state))
                            (knowledge-node-supports node))))
        (cond
          ((intersection states '(:changed :missing)) ':stale)
          ((member ':unverified states) ':unverified)
          ((member ':expired states) ':expired)
          ((member ':pending states) ':pending)
          ((member ':retracted states) ':retracted)
          (t ':unknown)))))

(-> knowledge--negation-identifier (knowledge-node) string)
(defun knowledge--negation-identifier (node)
  "Return the identifier of the claim contradicting NODE outright in its scope."
  (let ((proposition (knowledge-node-proposition node)))
    (knowledge-claim-identifier (if (knowledge-negation-p proposition)
                                    (second proposition)
                                    (list :not proposition))
                                :scope (knowledge-node-scope node)
                                :workspace (knowledge-node-workspace node))))


;;;; -- Bases --

(-> knowledge--basis-set (list) list)
(defun knowledge--basis-set (bases)
  "Return BASES as a canonical, duplicate-free basis set."
  (remove-if-not (lambda (basis) (member basis bases)) *knowledge-basis-order*))

(-> knowledge--add-basis-set (knowledge-node list) null)
(defun knowledge--add-basis-set (node set)
  "Add basis SET to NODE, keeping only minimal sets within the limit."
  (let ((sets (knowledge-node-bases node)))
    (unless (some (lambda (existing) (subsetp existing set)) sets)
      (setf (knowledge-node-bases node)
            (let ((kept (cons set (remove-if (lambda (existing) (subsetp set existing))
                                             sets))))
              (subseq kept 0 (min (length kept) *knowledge-basis-set-limit*))))))
  nil)

(-> knowledge-node-observed-p (knowledge-node &key (:accepted list)) boolean)
(defun knowledge-node-observed-p (node &key (accepted '(:observation :user-assertion)))
  "Return true when some path supporting NODE rests only on ACCEPTED bases.

An action policy can require claims independent of assumptions and model
proposals this way."
  (and (some (lambda (set) (subsetp set accepted)) (knowledge-node-bases node)) t))


;;;; -- Building the View --

(-> knowledge--view-node (knowledge-view string list memory-scope (option string)) knowledge-node)
(defun knowledge--view-node (view identifier proposition scope workspace)
  "Return VIEW's node IDENTIFIER, creating it for PROPOSITION when absent."
  (or (gethash identifier (knowledge-view-nodes view))
      (setf (gethash identifier (knowledge-view-nodes view))
            (make-instance 'knowledge-node
                           :identifier identifier
                           :proposition proposition
                           :scope scope
                           :workspace workspace
                           :schema (knowledge-base-schema
                                    (knowledge-view-base view)
                                    (knowledge-proposition-predicate proposition)
                                    :workspace (knowledge-view-workspace view))))))

(-> knowledge--index-supported (knowledge-view knowledge-node) null)
(defun knowledge--index-supported (view node)
  "Index supported NODE for rule matching, once."
  (let* ((proposition (knowledge-node-proposition node))
         (predicate (knowledge-proposition-predicate proposition)))
    (if (knowledge-node-negated-p node)
        (pushnew node (gethash predicate (knowledge-view-negated view)))
        (unless (member node (gethash predicate (knowledge-view-supported view)))
          (push node (gethash predicate (knowledge-view-supported view)))
          (loop for term in (knowledge-proposition-arguments proposition)
                for position from 0
                when (stringp term)
                  do (push node (gethash (list predicate position term)
                                         (knowledge-view-argument-index view)))))))
  nil)

(-> knowledge--evaluate-stored (knowledge-view evidence-check) null)
(defun knowledge--evaluate-stored (view check)
  "Add every visible stored claim to VIEW with the state of each support."
  (let ((base (knowledge-view-base view))
        (workspace (knowledge-view-workspace view)))
    (loop for entry being the hash-values of (knowledge-base-claims base)
          when (knowledge-visible-p (getf entry :scope) (getf entry :workspace) workspace)
            do (let ((node (knowledge--view-node view (getf entry :id)
                                                 (getf entry :proposition)
                                                 (getf entry :scope)
                                                 (getf entry :workspace))))
                 (dolist (support-identifier (getf entry :supports))
                   (let ((support (knowledge-base-support base support-identifier)))
                     (multiple-value-bind (state reason)
                         (evidence-check-support check base support)
                       (push (list :id support-identifier
                                   :state state
                                   :reason reason
                                   :basis (getf support :basis)
                                   :evidence (getf support :evidence))
                             (knowledge-node-supports node))
                       (when (eq state ':current)
                         (knowledge--add-basis-set node (list (getf support :basis)))))))
                 (when (knowledge-node-supported-p node)
                   (knowledge--index-supported view node)))))
  nil)

(-> knowledge--rule-order (list) list)
(defun knowledge--rule-order (rules)
  "Return RULES so that each runs after every rule concluding a predicate it reads."
  (let ((producers (make-hash-table :test #'equal))
        (visited (make-hash-table :test #'eq))
        (ordered nil))
    (dolist (item rules)
      (push item (gethash (knowledge-rule-conclusion-predicate (third item)) producers)))
    (labels ((visit (item)
               (unless (gethash item visited)
                 (setf (gethash item visited) t)
                 (dolist (predicate (knowledge-rule-premise-predicates (third item)))
                   (mapc #'visit (gethash predicate producers)))
                 (push item ordered))))
      (mapc #'visit rules))
    (nreverse ordered)))

(-> knowledge--relevant-rules (list list) list)
(defun knowledge--relevant-rules (rules goals)
  "Return the RULES that can contribute to predicates in GOALS, or all when GOALS is empty."
  (if (null goals)
      rules
      (let ((needed (make-hash-table :test #'equal))
            (pending (copy-list goals)))
        (loop while pending
              do (let ((predicate (pop pending)))
                   (unless (gethash predicate needed)
                     (setf (gethash predicate needed) t)
                     (dolist (item rules)
                       (when (string= (knowledge-rule-conclusion-predicate (third item))
                                      predicate)
                         (setf pending (append (knowledge-rule-premise-predicates (third item))
                                               pending)))))))
        (remove-if-not (lambda (item)
                         (gethash (knowledge-rule-conclusion-predicate (third item)) needed))
                       rules))))

(-> knowledge--candidates (knowledge-view list list) list)
(defun knowledge--candidates (view pattern environment)
  "Return supported nodes that could match positive PATTERN under ENVIRONMENT.

A bound or literal string argument selects candidates through the argument index."
  (let ((predicate (first pattern)))
    (loop for item in (rest pattern)
          for position from 0
          for value = (if (knowledge-variable-p item)
                          (rest (assoc item environment :test #'string=))
                          item)
          when (stringp value)
            do (return-from knowledge--candidates
                 (values (gethash (list predicate position value)
                                  (knowledge-view-argument-index view)))))
    (values (gethash predicate (knowledge-view-supported view)))))

(-> knowledge--unify (list list list) (values list boolean))
(defun knowledge--unify (pattern proposition environment)
  "Extend ENVIRONMENT so PATTERN's arguments match PROPOSITION's, or fail.

Return the extended environment and whether matching succeeded."
  (let ((result environment))
    (loop for item in (rest pattern)
          for term in (knowledge-proposition-arguments proposition)
          do (if (knowledge-variable-p item)
                 (let ((bound (assoc item result :test #'string=)))
                   (cond
                     ((null bound)
                      (push (cons item term) result))
                     ((not (knowledge-term= (rest bound) term))
                      (return-from knowledge--unify (values nil nil)))))
                 (unless (knowledge-term= item term)
                   (return-from knowledge--unify (values nil nil)))))
    (values result t)))

(-> knowledge--instantiate (list list) list)
(defun knowledge--instantiate (pattern environment)
  "Return PATTERN with its variables replaced by their values in ENVIRONMENT."
  (if (knowledge-negation-p pattern)
      (list :not (knowledge--instantiate (second pattern) environment))
      (cons (first pattern)
            (mapcar (lambda (item)
                      (if (knowledge-variable-p item)
                          (rest (assoc item environment :test #'string=))
                          item))
                    (rest pattern)))))

(-> knowledge--negation-supported-p (knowledge-view list list) boolean)
(defun knowledge--negation-supported-p (view pattern environment)
  "Return true when VIEW supports the explicit negation of PATTERN under ENVIRONMENT."
  (let ((atom (knowledge--instantiate pattern environment)))
    (and (some (lambda (node)
                 (knowledge-propositions-equal-p (second (knowledge-node-proposition node))
                                                 atom))
               (gethash (first atom) (knowledge-view-negated view)))
         t)))

(-> knowledge--record-error (knowledge-view list integer string) null)
(defun knowledge--record-error (view rule version message)
  "Retain one refused derivation of RULE VERSION with MESSAGE."
  (when (< (length (knowledge-view-errors view)) *knowledge-error-limit*)
    (pushnew (list :rule (getf (rest rule) :id) :version version :message message)
             (knowledge-view-errors view)
             :test #'equal))
  nil)

(-> knowledge--conclude (knowledge-view list list integer list list) null)
(defun knowledge--conclude (view entry rule version premises environment)
  "Derive RULE's conclusion for one complete match of PREMISES under ENVIRONMENT."
  (let ((table (make-hash-table :test #'equal)))
    (loop for (variable . value) in environment
          do (setf (gethash variable table) value))
    (handler-case
        (progn
          (dolist (binding (getf (rest rule) :bindings))
            (setf (gethash (first binding) table)
                  (knowledge-evaluate-expression (second binding) table)))
          (when (every (lambda (guard) (knowledge-guard-holds-p guard table))
                       (getf (rest rule) :guards))
            (let* ((pattern (getf (rest rule) :conclusion))
                   (proposition (knowledge--instantiate
                                 pattern
                                 (loop for variable being the hash-keys of table
                                         using (hash-value value)
                                       collect (cons variable value))))
                   (schema (knowledge-base-schema (knowledge-view-base view)
                                                  (knowledge-proposition-predicate proposition)
                                                  :workspace (knowledge-view-workspace view))))
              (unless (and schema (knowledge-proposition-valid-p proposition schema))
                (knowledge--fail ':evaluation proposition
                                 "The conclusion does not fit the schema of ~A."
                                 (knowledge-proposition-predicate proposition)))
              (let* ((workspace-p (or (eq (getf entry :scope) ':workspace)
                                      (some (lambda (premise)
                                              (eq (knowledge-node-scope premise) ':workspace))
                                            premises)))
                     (scope (if workspace-p ':workspace ':global))
                     (workspace (and workspace-p (knowledge-view-workspace view)))
                     (node (knowledge--view-node
                            view
                            (knowledge-claim-identifier proposition
                                                        :scope scope :workspace workspace)
                            proposition scope workspace))
                     (premise-identifiers (mapcar #'knowledge-node-identifier premises))
                     (justification
                       (knowledge-digest-identifier
                        #\j (list (getf entry :id) version premise-identifiers))))
                (unless (find justification (knowledge-node-justifications node)
                              :key (lambda (item) (getf item :id)) :test #'string=)
                  (incf (knowledge-view-matches view))
                  (push (list :id justification
                              :rule (getf entry :id)
                              :version version
                              :premises premise-identifiers
                              :bindings (loop for binding in (getf (rest rule) :bindings)
                                              collect (cons (first binding)
                                                            (gethash (first binding) table))))
                        (knowledge-node-justifications node))
                  (knowledge--add-basis-set
                   node
                   (knowledge--basis-set
                    (loop for premise in premises
                          append (first (knowledge-node-bases premise)))))
                  (knowledge--index-supported view node))))))
      (knowledge-error (condition)
        (knowledge--record-error view rule version (autolith-error-message condition)))))
  nil)

(-> knowledge--run-rule (knowledge-view list) null)
(defun knowledge--run-rule (view item)
  "Derive every conclusion of one admitted rule ITEM, (ENTRY VERSION RULE), within limits."
  (destructuring-bind (entry version rule) item
    (let ((positive (remove-if #'knowledge-negation-p (getf (rest rule) :premises)))
          (negative (remove-if-not #'knowledge-negation-p (getf (rest rule) :premises))))
      (labels ((exhausted-p ()
                 (or (>= (knowledge-view-matches view) *knowledge-match-limit*)
                     (>= (knowledge-view-steps view) *knowledge-step-limit*)))

               (match (patterns environment premises)
                 (cond
                   ((exhausted-p)
                    (pushnew (getf entry :id) (knowledge-view-incomplete view)
                             :test #'string=))
                   ((null patterns)
                    (when (every (lambda (pattern)
                                   (knowledge--negation-supported-p
                                    view (second pattern) environment))
                                 negative)
                      (knowledge--conclude view entry rule version (reverse premises)
                                           environment)))
                   (t
                    (dolist (node (knowledge--candidates view (first patterns) environment))
                      (when (exhausted-p)
                        (pushnew (getf entry :id) (knowledge-view-incomplete view)
                                 :test #'string=)
                        (return))
                      (incf (knowledge-view-steps view))
                      (multiple-value-bind (extended matched-p)
                          (knowledge--unify (first patterns) (knowledge-node-proposition node)
                                            environment)
                        (when matched-p
                          (match (rest patterns) extended (cons node premises)))))))))
        (match positive nil nil))))
  nil)

(-> knowledge--detect-conflicts (knowledge-view) null)
(defun knowledge--detect-conflicts (view)
  "Mark supported claims that disagree on a keyed value or contradict a negation."
  (let ((groups (make-hash-table :test #'equal)))
    (loop for node being the hash-values of (knowledge-view-nodes view)
          when (knowledge-node-supported-p node)
            do (if (knowledge-node-negated-p node)
                   (let ((positive (gethash (knowledge--negation-identifier node)
                                            (knowledge-view-nodes view))))
                     (when (and positive (knowledge-node-supported-p positive))
                       (pushnew (knowledge-node-identifier positive)
                                (knowledge-node-conflicts node) :test #'string=)
                       (pushnew (knowledge-node-identifier node)
                                (knowledge-node-conflicts positive) :test #'string=)))
                   (let ((key (and (knowledge-node-schema node)
                                   (knowledge-proposition-key (knowledge-node-proposition node)
                                                              (knowledge-node-schema node)))))
                     (when key
                       (push node (gethash (prin1-to-string key) groups))))))
    (loop for nodes being the hash-values of groups
          do (dolist (node nodes)
               (dolist (other nodes)
                 (unless (or (eq node other)
                             (knowledge-propositions-equal-p (knowledge-node-proposition node)
                                                             (knowledge-node-proposition other)))
                   (pushnew (knowledge-node-identifier other) (knowledge-node-conflicts node)
                            :test #'string=))))))
  nil)

(-> knowledge-evaluate
    (knowledge-base &key (:workspace (option string)) (:check evidence-check) (:goals list))
    knowledge-view)
(defun knowledge-evaluate (base &key workspace check goals)
  "Return the view of BASE from WORKSPACE, with evidence decided by CHECK.

GOALS, a list of predicates, limits rule evaluation to rules that can
contribute to them; by default every admitted rule runs. A view whose work
limits were reached lists the unfinished rules as incomplete; their absent
conclusions are unknown, never false."
  (let ((view (make-instance 'knowledge-view
                             :base base
                             :workspace workspace
                             :now (evidence-check-now check))))
    (knowledge--evaluate-stored view check)
    (dolist (item (knowledge--rule-order
                   (knowledge--relevant-rules
                    (knowledge-base-active-rules base :workspace workspace) goals)))
      (knowledge--run-rule view item))
    (knowledge--detect-conflicts view)
    view))

(-> knowledge-view-node (knowledge-view string) (option knowledge-node))
(defun knowledge-view-node (view identifier)
  "Return VIEW's evaluated claim IDENTIFIER, or NIL."
  (values (gethash identifier (knowledge-view-nodes view))))

(-> knowledge-view-holds (knowledge-view list) list)
(defun knowledge-view-holds (view pattern)
  "Return VIEW's supported claims matching PATTERN, a proposition with variables.

A negated PATTERN matches supported explicit negations."
  (let ((negated-p (knowledge-negation-p pattern))
        (atom (knowledge-proposition-atom pattern)))
    (remove-if-not
     (lambda (node)
       (nth-value 1 (knowledge--unify atom
                                      (knowledge-proposition-atom
                                       (knowledge-node-proposition node))
                                      nil)))
     (if negated-p
         (gethash (first atom) (knowledge-view-negated view))
         (gethash (first atom) (knowledge-view-supported view))))))
