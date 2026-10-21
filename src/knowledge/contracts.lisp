(in-package #:autolith)

;;;; -- Action Contracts --

;;; A contract describes what a tool call needs and does:
;;;
;;;   (:contract :id "send-report" :tool "mcp__mail.send"
;;;    :when (("to" "?recipient"))
;;;    :requires (("report.ready" "?recipient"))
;;;    :effects (("report.sent" "?recipient"))
;;;    :observes ((:predicate "mail.message" :arguments ("$arg.to" "$result.id"))))
;;;
;;; Before a matching call runs, its requirements must hold; a requirement that
;;; fails or cannot be decided refuses the call with the reason. After the call
;;; succeeds, its effects are recorded as the model's predictions, which no
;;; fact gate accepts, and its observations are read from the call's arguments
;;; and its JSON result and recorded as observations of that call. This is how
;;; structured tool output, including MCP results, becomes facts: only through
;;; a contract that names the predicate for each value.

(defparameter *knowledge-contract-pattern-limit* 8
  "The most requirements, effects, or observations one contract lists.")


;;;; -- Validation --

(-> knowledge--reference-p (t) boolean)
(defun knowledge--reference-p (value)
  "Return true when VALUE refers to a call argument or a result field."
  (and (stringp value)
       (or (uiop:string-prefix-p "$arg." value)
           (uiop:string-prefix-p "$result." value)
           (string= value "$result"))
       t))

(-> knowledge-contract-p (t) boolean)
(defun knowledge-contract-p (contract)
  "Return true when CONTRACT has the structure of a validated contract."
  (and (proper-list-p contract)
       (eq (first contract) :contract)
       (evenp (length (rest contract)))
       (let ((properties (rest contract)))
         (and (knowledge-rule-name-p (getf properties :id))
              (non-empty-string-p (getf properties :tool))
              (every (lambda (entry)
                       (and (consp entry) (non-empty-string-p (first entry))))
                     (getf properties :when))
              (every #'consp (getf properties :requires))
              (every #'consp (getf properties :effects))
              (every (lambda (basis) (typep basis 'knowledge-basis)) (getf properties :accepts))
              (every (lambda (observation)
                       (and (proper-list-p observation)
                            (knowledge-predicate-name-p (getf observation :predicate))
                            (proper-list-p (getf observation :arguments))))
                     (getf properties :observes))))
       t))

(-> knowledge-make-contract
    (&key (:id string) (:tool string) (:when t) (:requires sequence) (:effects sequence)
          (:observes sequence) (:accepts sequence) (:description (option string))
          (:schema-function function))
    list)
(defun knowledge-make-contract
    (&key id tool when requires effects observes accepts description schema-function)
  "Return a validated contract built from input forms.

WHEN maps argument names to required values or to variables they bind.
Requirement and effect variables must be bound by WHEN or a positive
requirement. Observation arguments are values, bound variables, or references
such as $arg.to and $result.id. ACCEPTS names the bases a requirement may rest
on, by default observations and the user's statements; naming model-proposal
lets the predicted effects of earlier calls satisfy it."
  (unless (knowledge-rule-name-p id)
    (knowledge--fail ':invalid-contract id
                     "Contract names are lowercase letters, digits, hyphens, and dots."))
  (unless (non-empty-string-p tool)
    (knowledge--fail ':invalid-contract tool "A contract names the tool it describes, such as shell.run."))
  (flet ((patterns (value)
           (let ((items (coerce (or value #()) 'list)))
             (when (> (length items) *knowledge-contract-pattern-limit*)
               (knowledge--fail ':invalid-contract id "A contract lists at most ~D of each kind."
                                *knowledge-contract-pattern-limit*))
             (mapcar (lambda (pattern)
                       (knowledge--pattern-from-input pattern schema-function :negatable t))
                     items))))
    (let* ((conditions
             (cond
               ((null when) nil)
               ((hash-table-p when)
                (loop for name being the hash-keys of when using (hash-value value)
                      collect (cons name value)))
               (t (knowledge--fail ':invalid-contract when
                                   "when maps argument names to values or variables."))))
           (required (patterns requires))
           (bound (append (remove-if-not #'knowledge-variable-p (mapcar #'rest conditions))
                          (loop for pattern in required
                                unless (knowledge-negation-p pattern)
                                  append (knowledge--pattern-variables pattern))))
           (predicted (patterns effects))
           (observed
             (mapcar (lambda (observation)
                       (unless (hash-table-p observation)
                         (knowledge--fail ':invalid-contract observation
                                          "Each observation has a predicate and its arguments."))
                       (let* ((predicate (gethash "predicate" observation))
                              (schema (funcall schema-function predicate))
                              (arguments (coerce (gethash "arguments" observation #()) 'list)))
                         (unless schema
                           (knowledge--fail ':unknown-predicate predicate
                                            "Define predicate ~A before using it in a pattern."
                                            predicate))
                         (unless (= (length arguments) (length (knowledge-schema-arguments schema)))
                           (knowledge--fail ':invalid-contract predicate "~A takes ~D arguments."
                                            predicate (length (knowledge-schema-arguments schema))))
                         (list :predicate predicate
                               :arguments
                               (mapcar (lambda (argument)
                                         (cond
                                           ((knowledge--reference-p argument) argument)
                                           ((knowledge-variable-p argument)
                                            (unless (member argument bound :test #'string=)
                                              (knowledge--fail ':invalid-contract argument
                                                               "~A is not bound by when or a requirement."
                                                               argument))
                                            argument)
                                           (t argument)))
                                       arguments))))
                     (coerce (or observes #()) 'list))))
      (dolist (pattern (append (remove-if-not #'knowledge-negation-p required) predicted))
        (dolist (variable (knowledge--pattern-variables pattern))
          (unless (member variable bound :test #'string=)
            (knowledge--fail ':invalid-contract variable
                             "~A is not bound by when or a positive requirement." variable))))
      (list :contract :id id :tool tool :when conditions :requires required
            :effects predicted :observes observed
            :accepts (knowledge--accepted-bases accepts id)
            :description description))))

(-> knowledge--accepted-bases (t string) list)
(defun knowledge--accepted-bases (accepts identifier)
  "Return the bases ACCEPTS names, input text or keywords, or NIL for the default."
  (let ((items (coerce (or accepts #()) 'list)))
    (mapcar (lambda (item)
              (or (find item '(:observation :user-assertion :assumption :model-proposal)
                        :test (lambda (value basis)
                                (or (eq value basis)
                                    (and (stringp value)
                                         (string-equal value (symbol-name basis))))))
                  (knowledge--fail ':invalid-contract identifier
                                   "accepts lists observation, user-assertion, assumption, or model-proposal.")))
            items)))


;;;; -- Stored Contracts --

(-> knowledge-base-contracts (knowledge-base string &key (:workspace (option string))) list)
(defun knowledge-base-contracts (base tool &key workspace)
  "Return the contracts describing TOOL visible from WORKSPACE."
  (loop for entry being the hash-values of (knowledge-base-contracts-table base)
        for contract = (getf entry :contract)
        when (and (knowledge-visible-p (getf entry :scope) (getf entry :workspace) workspace)
                  (string= (getf (rest contract) :tool) tool))
          collect contract))

(-> knowledge-define-contract
    (configuration &key (:id string) (:tool string) (:when t) (:requires sequence)
                        (:effects sequence) (:observes sequence) (:accepts sequence)
                        (:description (option string)) (:scope memory-scope)
                        (:source-conversation (option string)))
    (values string boolean))
(defun knowledge-define-contract
    (configuration &key id tool when requires effects observes accepts description
                        (scope ':workspace) source-conversation)
  "Validate and record contract ID, replacing an earlier one of that name in SCOPE.

Return its ID and whether the definition changed."
  (let ((workspace (knowledge--owner configuration scope))
        (viewer (knowledge--workspace configuration)))
    (values-list
     (knowledge-transact
      configuration
      (lambda (base)
        (let* ((contract (knowledge-make-contract
                          :id id :tool tool :when when :requires requires :effects effects
                          :observes observes :accepts accepts :description description
                          :schema-function
                          (lambda (predicate)
                            (knowledge-base-schema base predicate
                                                   :workspace (and (eq scope ':workspace) viewer)))))
               (existing (gethash (list scope workspace id) (knowledge-base-contracts-table base))))
          (if (equal (getf existing :contract) contract)
              (values nil (list id nil) nil)
              (values (list (list :contract-defined
                                  :version *knowledge-format-version*
                                  :scope scope
                                  :workspace workspace
                                  :contract contract
                                  :time (get-universal-time)
                                  :source-conversation source-conversation))
                      (list id t)
                      t))))))))


;;;; -- Calls --

(-> knowledge--json-path (t list) (values t boolean))
(defun knowledge--json-path (value path)
  "Return the JSON value at PATH of keys and indexes below VALUE, and whether it exists."
  (if (null path)
      (values value t)
      (let ((key (first path)))
        (cond
          ((and (hash-table-p value) (nth-value 1 (gethash key value)))
           (knowledge--json-path (gethash key value) (rest path)))
          ((and (vectorp value) (not (stringp value))
                (every #'digit-char-p key) (plusp (length key))
                (< (parse-integer key) (length value)))
           (knowledge--json-path (aref value (parse-integer key)) (rest path)))
          (t (values nil nil))))))

(-> knowledge--reference-value (string hash-table t) (values t boolean))
(defun knowledge--reference-value (reference arguments result)
  "Return the value REFERENCE names in the call ARGUMENTS or its decoded RESULT."
  (let* ((rooted (uiop:string-prefix-p "$arg." reference))
         (path (uiop:split-string (subseq reference (if rooted 5 (min 8 (length reference))))
                                  :separator '(#\.))))
    (if rooted
        (knowledge--json-path arguments path)
        (if (string= reference "$result")
            (values result t)
            (knowledge--json-path result path)))))

(-> knowledge--contract-bindings (list hash-table) (values list boolean))
(defun knowledge--contract-bindings (contract arguments)
  "Return the variables CONTRACT's conditions bind from call ARGUMENTS, and whether it matches."
  (let ((environment nil))
    (dolist (condition (getf (rest contract) :when) (values environment t))
      (multiple-value-bind (value present-p) (gethash (first condition) arguments)
        (cond
          ((not present-p)
           (return (values nil nil)))
          ((knowledge-variable-p (rest condition))
           (push (cons (rest condition) value) environment))
          ((not (equal (rest condition) value))
           (return (values nil nil))))))))

(-> knowledge--typed-environment (list list function) list)
(defun knowledge--typed-environment (patterns environment schema-function)
  "Return ENVIRONMENT with JSON values read by the schema types of the PATTERNS using them."
  (mapcar
   (lambda (binding)
     (let ((type (loop for pattern in patterns
                       for atom = (knowledge-rule-premise-atom pattern)
                       for schema = (funcall schema-function (first atom))
                       for position = (and schema (position (first binding) (rest atom)
                                                            :test #'equal))
                       when position
                         return (getf (nth position (knowledge-schema-arguments schema)) :type))))
       (if type
           (cons (first binding) (knowledge-term-from-input (rest binding) type))
           binding)))
   environment))

(-> knowledge-contracts-for-call (configuration string hash-table) list)
(defun knowledge-contracts-for-call (configuration tool arguments)
  "Return (CONTRACT ENVIRONMENT) for each contract describing this call of TOOL."
  (let* ((base (knowledge-base-read configuration))
         (workspace (knowledge--workspace configuration))
         (schema-function (lambda (predicate)
                            (knowledge-base-schema base predicate :workspace workspace))))
    (loop for contract in (knowledge-base-contracts base tool :workspace workspace)
          for (environment matched-p) = (multiple-value-list
                                         (knowledge--contract-bindings contract arguments))
          when matched-p
            collect (list contract
                          (knowledge--typed-environment
                           (append (getf (rest contract) :requires)
                                   (getf (rest contract) :effects))
                           environment schema-function)))))

(-> knowledge-contract-refusal (configuration list) (option string))
(defun knowledge-contract-refusal (configuration matches)
  "Return why the call described by contract MATCHES must not run, or NIL when it may.

A requirement holds only on undisputed facts resting on the bases the contract
accepts, by default observations and the user's statements."
  (let ((view (knowledge-view-current configuration)))
    (loop for (contract environment) in matches
          for accepted = (or (getf (rest contract) :accepts) *knowledge-established-bases*)
          for requires = (mapcar (lambda (pattern)
                                   (knowledge--instantiate-pattern pattern environment))
                                 (getf (rest contract) :requires))
          when requires
            do (multiple-value-bind (solutions cut-p)
                   (knowledge-view-solve view requires
                                         :limit 1
                                         :test (lambda (node)
                                                 (knowledge-node-admissible-p
                                                  view node :accepted accepted)))
                 (unless solutions
                   (return
                     (format nil "Contract ~A requires ~{~A~^ and ~}, which ~A. The call was not made."
                             (getf (rest contract) :id)
                             (mapcar #'knowledge-pattern-text requires)
                             (cond
                               (cut-p "could not be decided within the search limit")
                               ((knowledge-view-solve view requires :limit 1)
                                (format nil "holds only on disputed facts or on bases it does not accept (it accepts ~{~(~A~)~^, ~})"
                                        accepted))
                               (t "does not hold")))))))))

(-> knowledge-contract-record (configuration list hash-table t &key (:tool string)
                                                                       (:conversation (option string)))
    list)
(defun knowledge-contract-record (configuration matches arguments result &key tool conversation)
  "Record the predicted effects and observed facts of a successful described call.

Return the asserted claim identifiers. A value missing from the result, or not
fitting its predicate, skips that observation rather than failing the call."
  (let* ((decoded (ignore-errors (json-decode (tool-result-content result))))
         (base (knowledge-base-read configuration))
         (workspace (knowledge--workspace configuration))
         (claims nil))
    (loop for (contract environment) in matches
          for identifier = (getf (rest contract) :id)
          do (dolist (effect (getf (rest contract) :effects))
               (handler-case
                   (push (knowledge-assert
                          configuration (knowledge--instantiate-pattern effect environment)
                          :evidence (knowledge-observe
                                     :source (list :tool :name tool :conversation conversation
                                                   :call identifier)
                                     :adapter (format nil "contract/~A/predicted" identifier))
                          :basis ':model-proposal
                          :source-conversation conversation)
                         claims)
                 (knowledge-error () nil)))
             (dolist (observation (getf (rest contract) :observes))
               (handler-case
                   (let* ((schema (knowledge-base-schema base (getf observation :predicate)
                                                         :workspace workspace))
                          (inputs
                            (mapcar (lambda (argument)
                                      (cond
                                        ((knowledge--reference-p argument)
                                         (multiple-value-bind (value present-p)
                                             (knowledge--reference-value argument arguments decoded)
                                           (unless present-p
                                             (knowledge--fail ':missing-value argument
                                                              "~A is not in the call." argument))
                                           value))
                                        ((knowledge-variable-p argument)
                                         (rest (assoc argument environment :test #'string=)))
                                        (t argument)))
                                    (getf observation :arguments))))
                     (push (knowledge-assert
                            configuration (knowledge-make-proposition schema inputs)
                            :evidence (knowledge-observe
                                       :source (list :tool :name tool :conversation conversation
                                                     :call identifier)
                                       :digest (memory-source-text-digest
                                                (tool-result-content result))
                                       :excerpt (tool-result-content result)
                                       :adapter (format nil "contract/~A" identifier))
                            :basis ':observation
                            :source-conversation conversation)
                           claims))
                 (knowledge-error () nil))))
    (nreverse claims)))
