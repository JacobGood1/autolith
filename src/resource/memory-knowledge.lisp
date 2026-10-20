(in-package #:autolith)

;;;; -- Structured Knowledge Resources --

;;; Structured knowledge is read and written through memory: resources, so the
;;; model meets one memory system. memory:facts lists the facts holding in this
;;; workspace and accepts fact, predicate, and rule operations; memory:claim/ID
;;; explains one claim; memory:predicates and memory:rules list the vocabulary
;;; and the rules. Every read evaluates locally and costs no model request.

(defparameter *memory-knowledge-default-results* 16
  "The facts one memory:facts read lists unless max-results asks for more.")

(defparameter *memory-knowledge-maximum-results* 50
  "The most facts one memory:facts read lists.")

(defclass memory-knowledge-resource (memory-resource)
  ((view-kind
    :initarg :view-kind
    :reader memory-knowledge-resource-view-kind
    :type keyword
    :documentation "Which knowledge view: :FACTS, :CLAIM, :PREDICATES, :RULES, or :CHANGES.")
   (claim
    :initarg :claim
    :initform nil
    :reader memory-knowledge-resource-claim
    :type (option string)
    :documentation "The claim a :CLAIM resource explains."))
  (:documentation "A structured-knowledge view within the memory: scheme."))


;;;; -- Resolution --

(-> memory-knowledge-resolve (string) (option memory-knowledge-resource))
(defun memory-knowledge-resolve (identifier)
  "Return the knowledge resource memory:IDENTIFIER names, or NIL for another kind."
  (flet ((make (kind &optional claim)
           (make-instance 'memory-knowledge-resource
                          :uri (format nil "memory:~A" identifier)
                          :identifier identifier
                          :view-kind kind
                          :claim claim)))
    (cond
      ((string= identifier "facts") (make ':facts))
      ((string= identifier "predicates") (make ':predicates))
      ((string= identifier "rules") (make ':rules))
      ((string= identifier "changes") (make ':changes))
      ((and (uiop:string-prefix-p "claim/" identifier)
            (> (length identifier) (length "claim/")))
       (make ':claim (subseq identifier (length "claim/"))))
      (t nil))))

(defmethod resource-capabilities
    ((resource memory-knowledge-resource) (context tool-context))
  "Accept edits only on memory:facts."
  (declare (ignore context))
  (if (eq (memory-knowledge-resource-view-kind resource) ':facts)
      '(:read :edit)
      '(:read)))


;;;; -- Evaluation --

(-> memory-knowledge-view (tool-context &key (:goals list)) knowledge-view)
(defun memory-knowledge-view (context &key goals)
  "Return CONTEXT's structured knowledge evaluated from its workspace now."
  (let ((configuration (tool-context-configuration context)))
    (knowledge-evaluate (knowledge-base-read configuration)
                        :workspace (knowledge--workspace configuration)
                        :check (evidence-check-create
                                :memories (memory-list configuration :visibility ':all))
                        :goals goals)))


;;;; -- Rendering --

(-> memory-knowledge--fact-line (knowledge-view knowledge-node) string)
(defun memory-knowledge--fact-line (view node)
  "Return NODE as one compact JSON line for the model."
  (let ((line (json-object
               "id" (knowledge-node-identifier node)
               "fact" (knowledge-node-text node)
               "predicate" (knowledge-proposition-predicate (knowledge-node-proposition node))
               "arguments" (map 'vector #'knowledge-term->json
                                (knowledge-proposition-arguments
                                 (knowledge-node-proposition node)))
               "basis" (map 'vector
                            (lambda (set)
                              (format nil "~{~(~A~)~^+~}" set))
                            (knowledge-node-bases node)))))
    (when (knowledge-node-negated-p node)
      (setf (gethash "negated" line) t))
    (when (knowledge-node-derived-p node)
      (let ((justification (first (knowledge-node-justifications node))))
        (setf (gethash "derived" line)
              (format nil "~A v~D" (getf justification :rule) (getf justification :version)))))
    (unless (eq (knowledge-node-status view node) ':supported)
      (setf (gethash "status" line)
            (string-downcase (symbol-name (knowledge-node-status view node)))))
    (json-encode line)))

(-> memory-knowledge--matches-p (knowledge-node list) boolean)
(defun memory-knowledge--matches-p (node terms)
  "Return true when every lowercase query term appears in NODE's text or arguments."
  (let ((haystack (string-downcase
                   (format nil "~A ~A"
                           (knowledge-node-text node)
                           (knowledge-node-proposition node)))))
    (and (every (lambda (term) (search term haystack)) terms) t)))

(-> memory-knowledge--render-facts
    (knowledge-view &key (:query (option string)) (:maximum-results integer))
    (values string list))
(defun memory-knowledge--render-facts (view &key query maximum-results)
  "Return the fact listing for VIEW and the identifiers it shows.

Conflicted facts come first, then the rest by predicate and text."
  (let* ((terms (and query (memory--search-terms query)))
         (facts (sort (loop for node being the hash-values of (knowledge-view-nodes view)
                            when (and (knowledge-node-supported-p node)
                                      (or (null terms) (memory-knowledge--matches-p node terms)))
                              collect node)
                      (lambda (left right)
                        (let ((left-conflicted (and (knowledge-node-conflicts left) t))
                              (right-conflicted (and (knowledge-node-conflicts right) t)))
                          (if (eq left-conflicted right-conflicted)
                              (string< (knowledge-node-text left) (knowledge-node-text right))
                              left-conflicted)))))
         (shown (subseq facts 0 (min maximum-results (length facts)))))
    (values
     (with-output-to-string (stream)
       (format stream "collection: facts~@[ matching ~S~]~%count: ~D~:[~; (showing ~D; narrow with query or raise max-results)~]~%"
               query (length facts) (> (length facts) (length shown)) (length shown))
       (when (knowledge-view-incomplete view)
         (format stream "incomplete: rules ~{~A~^, ~} reached a work limit; missing conclusions are unknown, not false~%"
                 (knowledge-view-incomplete view)))
       (dolist (error (knowledge-view-errors view))
         (format stream "rule ~A v~D could not derive: ~A~%"
                 (getf error :rule) (getf error :version) (getf error :message)))
       (if shown
           (format stream "~%~{~A~%~}Read memory:claim/ID for why a fact holds."
                   (mapcar (lambda (node) (memory-knowledge--fact-line view node)) shown))
           (format stream "~%No facts hold~:[~; for this query~]." query)))
     (mapcar #'knowledge-node-identifier shown))))

(-> memory-knowledge--render-support (list stream integer) null)
(defun memory-knowledge--render-support (support stream indent)
  "Write one explained SUPPORT to STREAM at INDENT."
  (format stream "~vTsupport ~A: ~(~A~), ~A (~A)~%"
          indent
          (getf support :support)
          (getf support :basis)
          (getf support :text)
          (knowledge-applicability-text (getf support :state) (getf support :reason)))
  nil)

(-> memory-knowledge--render-tree (list stream integer) null)
(defun memory-knowledge--render-tree (tree stream indent)
  "Write one explanation TREE to STREAM, nesting premises at INDENT."
  (cond
    ((getf tree :repeated)
     (format stream "~vT~A (shown above)~%" indent (getf tree :claim)))
    ((getf tree :more)
     (format stream "~vT~A ~A: read memory:claim/~A~%"
             indent (getf tree :claim) (getf tree :text) (getf tree :claim)))
    (t
     (format stream "~vT~A ~A [~(~A~), ~(~A~)]~@[ bases ~{~{~(~A~)~^+~}~^ or ~}~]~%"
             indent (getf tree :claim) (or (getf tree :text) "unknown claim")
             (getf tree :status) (getf tree :applicability) (getf tree :bases))
     (dolist (support (getf tree :supports))
       (memory-knowledge--render-support support stream (+ indent 2)))
     (dolist (justification (getf tree :justifications))
       (format stream "~vTderived by rule ~A v~D~@[ with ~{~A~^, ~}~] from:~%"
               (+ indent 2) (getf justification :rule) (getf justification :version)
               (mapcar (lambda (binding)
                         (format nil "~A = ~A" (first binding)
                                 (knowledge-term-display (rest binding))))
                       (getf justification :bindings)))
       (dolist (premise (getf justification :premises))
         (memory-knowledge--render-tree premise stream (+ indent 4))))
     (when (getf tree :conflicts)
       (format stream "~vTconflicts with ~{~A~^, ~}~%" (+ indent 2) (getf tree :conflicts)))))
  nil)

(-> memory-knowledge--render-changes (configuration string knowledge-view) string)
(defun memory-knowledge--render-changes (configuration viewer view)
  "Return what changed for VIEWER since it last read memory:changes, and record this read."
  (multiple-value-bind (changes previous first-p)
      (knowledge-viewer-changes configuration viewer view)
    (cond
      (first-p
       (format nil "First look: ~D fact~:P hold now. Later reads of memory:changes report what changed since this one."
               (length (knowledge-view-snapshot view))))
      (changes
       (format nil "Changes since ~A:~%~{~A~%~}"
               (memory--timestamp-string previous)
               (memory-knowledge--change-lines changes)))
      (t
       (format nil "Nothing changed since ~A." (memory--timestamp-string previous))))))

(-> memory-knowledge--render-claim (knowledge-view string) string)
(defun memory-knowledge--render-claim (view identifier)
  "Return why claim IDENTIFIER holds or does not, as an indented tree."
  (with-output-to-string (stream)
    (format stream "claim: ~A~%" identifier)
    (memory-knowledge--render-tree (first (knowledge-explain view identifier)) stream 0)))

(-> memory-knowledge--render-predicates (knowledge-base (option string)) string)
(defun memory-knowledge--render-predicates (base workspace)
  "Return the predicates visible from WORKSPACE with their arguments and templates."
  (let ((entries (sort (loop for key being the hash-keys of (knowledge-base-schemas base)
                               using (hash-value value)
                             when (knowledge-visible-p (first key) (second key) workspace)
                               collect (list key value))
                       #'string< :key (lambda (entry) (third (first entry))))))
    (with-output-to-string (stream)
      (format stream "collection: predicates~%count: ~D~%" (length entries))
      (dolist (entry entries)
        (destructuring-bind ((scope owner predicate) (schema version)) entry
          (declare (ignore owner))
          (format stream "~%~A(~{~A~^, ~}) v~D ~(~A~)~@[ key ~{~A~^, ~}~]~@[ template ~S~]~@[: ~A~]"
                  predicate
                  (mapcar (lambda (argument)
                            (format nil "~A ~(~A~)~@[ in ~A~]"
                                    (getf argument :name) (getf argument :type)
                                    (getf argument :unit)))
                          (knowledge-schema-arguments schema))
                  version scope
                  (mapcar (lambda (position)
                            (getf (nth position (knowledge-schema-arguments schema)) :name))
                          (knowledge-schema-key schema))
                  (getf (rest schema) :template)
                  (getf (rest schema) :description))))
      (unless entries
        (format stream "~%No predicates are defined. Define one with predicate-define on memory:facts.")))))

(-> memory-knowledge--pattern-text (list) string)
(defun memory-knowledge--pattern-text (pattern)
  "Return a rule PATTERN as compact text."
  (if (knowledge-negation-p pattern)
      (format nil "not ~A" (memory-knowledge--pattern-text (second pattern)))
      (format nil "~A(~{~A~^, ~})" (first pattern)
              (mapcar (lambda (item)
                        (if (knowledge-variable-p item) item (knowledge-term-display item)))
                      (rest pattern)))))

(-> memory-knowledge--render-rules (knowledge-base (option string)) string)
(defun memory-knowledge--render-rules (base workspace)
  "Return the rules visible from WORKSPACE with each version's state."
  (let ((entries (sort (knowledge-base-rule-entries base :workspace workspace)
                       #'string< :key (lambda (entry) (getf entry :id)))))
    (with-output-to-string (stream)
      (format stream "collection: rules~%count: ~D~%" (length entries))
      (dolist (entry entries)
        (let* ((latest (first (last (getf entry :versions))))
               (rule (getf latest :rule)))
          (format stream "~%~A v~D ~(~A~): ~:[proposed, awaiting admission~;~:*active version ~D~]~%  ~{~A~^ and ~}~@[ where ~{~A~^, ~}~]~@[ with ~{~A~^, ~}~] => ~A~@[~%  ~A~]~%"
                  (getf entry :id) (getf latest :version) (getf entry :scope)
                  (getf entry :active)
                  (mapcar #'memory-knowledge--pattern-text (getf (rest rule) :premises))
                  (mapcar (lambda (guard) (format nil "~A ~S ~S" (first guard) (second guard)
                                                  (third guard)))
                          (getf (rest rule) :guards))
                  (mapcar (lambda (binding) (format nil "~A = ~S" (first binding) (second binding)))
                          (getf (rest rule) :bindings))
                  (memory-knowledge--pattern-text (getf (rest rule) :conclusion))
                  (getf (rest rule) :description))))
      (unless entries
        (format stream "~%No rules are defined.")))))


;;;; -- Observation --

(-> memory-knowledge-observe
    (memory-knowledge-resource tool-context
     &key (:query (option string)) (:maximum-results (option integer)))
    memory-observation)
(defun memory-knowledge-observe (resource context &key query maximum-results)
  "Return RESOURCE's current rendering as a revisioned memory observation."
  (let* ((configuration (tool-context-configuration context))
         (base (knowledge-base-read configuration))
         (workspace (knowledge--workspace configuration))
         (kind (memory-knowledge-resource-view-kind resource))
         (content
           (ecase kind
             (:facts
              (memory-knowledge--render-facts
               (memory-knowledge-view context)
               :query query
               :maximum-results (min (or maximum-results *memory-knowledge-default-results*)
                                     *memory-knowledge-maximum-results*)))
             (:claim
              (memory-knowledge--render-claim (memory-knowledge-view context)
                                              (memory-knowledge-resource-claim resource)))
             (:predicates
              (memory-knowledge--render-predicates base workspace))
             (:rules
              (memory-knowledge--render-rules base workspace))
             (:changes
              (memory-knowledge--render-changes
               configuration
               (format nil "conversation:~A"
                       (conversation-identifier (tool-context-conversation context)))
               (memory-knowledge-view context)))))
         (snapshot (list :kind kind
                         :identifier (memory-resource-identifier resource)
                         :workspace workspace
                         :revision (knowledge-base-revision base)
                         :content content)))
    (make-instance 'memory-observation
                   :uri        (resource-uri resource)
                   :revision   (resource-readable-snapshot-digest
                                *memory-resource-digest-key* snapshot)
                   :content    content
                   :metadata   (list :knowledge kind :query query
                                     :maximum-results maximum-results)
                   :identifier (memory-resource-identifier resource)
                   :kind       kind
                   :snapshot   snapshot)))

(defmethod resource-observe
    ((resource memory-knowledge-resource) (context tool-context))
  "Observe one structured-knowledge view."
  (memory-knowledge-observe resource context))

(defmethod resource-tool-read
    ((resource memory-knowledge-resource) (tool resource-read-tool)
     (context tool-context) (arguments hash-table))
  "Read one structured-knowledge view, with an optional query on memory:facts."
  (declare (ignore tool))
  (let ((query (tool-argument arguments "query"))
        (maximum (tool-argument arguments "max-results")))
    (when (or (nth-value 1 (gethash "start-line" arguments))
              (nth-value 1 (gethash "line-count" arguments)))
      (memory-resource--fail "memory: knowledge views are read in full and do not accept line windows."))
    (when (and (or query maximum)
               (not (eq (memory-knowledge-resource-view-kind resource) ':facts)))
      (memory-resource--fail "Only memory:facts accepts query and max-results."))
    (when (and query (not (non-empty-string-p query)))
      (memory-resource--fail "The query must be non-empty text."))
    (when (and maximum (not (and (integerp maximum) (plusp maximum))))
      (memory-resource--fail "max-results must be a positive integer."))
    (handler-case
        (tool-success
         (resource-item-read-result
          (resource-observation-state-ensure
           (tool-context-conversation context)
           (memory-knowledge-observe resource context
                                     :query query :maximum-results maximum))))
      (knowledge-error (condition)
        (memory-resource--fail "~A" (autolith-error-message condition))))))


;;;; -- Operations --

(defparameter *memory-knowledge-operation-limit* 16
  "The most operations one memory:facts edit applies.")

(defparameter *memory-knowledge-type-names*
  '("entity" "string" "number" "boolean" "instant" "date" "duration" "quantity")
  "The argument type names a predicate definition accepts.")

(-> memory-knowledge--fields (hash-table string list list) null)
(defun memory-knowledge--fields (operation name allowed required)
  "Require OPERATION, a NAME operation, to use only ALLOWED and all REQUIRED fields."
  (loop for field being the hash-keys of operation
        unless (member field allowed :test #'string=)
          do (memory-resource--fail "~A does not accept field ~A; it accepts ~{~A~^, ~}."
                                    name field allowed))
  (dolist (field required)
    (unless (nth-value 1 (gethash field operation))
      (memory-resource--fail "~A requires ~A." name field)))
  nil)

(-> memory-knowledge--scope (hash-table) memory-scope)
(defun memory-knowledge--scope (operation)
  "Return OPERATION's scope, workspace unless it names global."
  (let ((scope (gethash "scope" operation "workspace")))
    (cond
      ((equal scope "workspace") ':workspace)
      ((equal scope "global") ':global)
      (t (memory-resource--fail "scope must be workspace or global.")))))

(-> memory-knowledge--list (t string) list)
(defun memory-knowledge--list (value field)
  "Return JSON array VALUE of FIELD as a list."
  (unless (vectorp value)
    (memory-resource--fail "~A must be an array." field))
  (coerce value 'list))

(-> memory-knowledge--define (configuration hash-table) string)
(defun memory-knowledge--define (configuration operation)
  "Apply one predicate-define OPERATION and describe the result."
  (memory-knowledge--fields operation "predicate-define"
                            '("op" "predicate" "arguments" "key" "template" "description" "scope")
                            '("predicate" "arguments"))
  (let ((schema
          (knowledge-make-schema
           :predicate (gethash "predicate" operation)
           :arguments
           (mapcar (lambda (argument)
                     (unless (hash-table-p argument)
                       (memory-resource--fail "Each argument is an object with name and type."))
                     (let ((type (gethash "type" argument)))
                       (unless (member type *memory-knowledge-type-names* :test #'equal)
                         (memory-resource--fail "Argument type ~S must be one of ~{~A~^, ~}."
                                                type *memory-knowledge-type-names*))
                       (append (list :name (gethash "name" argument)
                                     :type (intern (string-upcase type) :keyword))
                               (when (gethash "unit" argument)
                                 (list :unit (gethash "unit" argument))))))
                   (memory-knowledge--list (gethash "arguments" operation) "arguments"))
           :key (and (gethash "key" operation)
                     (memory-knowledge--list (gethash "key" operation) "key"))
           :template (gethash "template" operation)
           :description (gethash "description" operation))))
    (multiple-value-bind (schema version)
        (knowledge-define-schema configuration schema
                                 :scope (memory-knowledge--scope operation))
      (format nil "Defined ~A version ~D." (knowledge-schema-predicate schema) version))))

(-> memory-knowledge--evidence (tool-context list) list)
(defun memory-knowledge--evidence (context source)
  "Return knowledge evidence for SOURCE, a checked memory source record."
  (let ((properties (rest source))
        (workspace (knowledge--workspace (tool-context-configuration context))))
    (ecase (first source)
      (:file
       (let* ((pathname (uiop:parse-native-namestring (getf properties :path)))
              (relative (uiop:subpathp pathname (uiop:ensure-directory-pathname workspace))))
         (knowledge-observe :source (list :file
                                          :path (if relative
                                                    (uiop:unix-namestring relative)
                                                    (getf properties :path))
                                          :relative (and relative t)
                                          :quote (getf properties :quote))
                            :workspace (and relative workspace)
                            :digest (getf properties :digest)
                            :excerpt (getf properties :quote)
                            :adapter "file/1"
                            :time (getf properties :observed-at))))
      (:url
       (knowledge-observe :source (list :url :url (getf properties :url)
                                        :quote (getf properties :quote))
                          :excerpt (getf properties :quote)
                          :adapter "web/1"
                          :time (getf properties :observed-at)))
      (:user
       (knowledge-observe :source (list :user :quote (getf properties :quote)
                                        :conversation (getf properties :conversation))
                          :excerpt (getf properties :quote)
                          :adapter "user/1"
                          :time (getf properties :observed-at))))))

(-> memory-knowledge--time (hash-table string) (option timestamp))
(defun memory-knowledge--time (operation field)
  "Return OPERATION's optional instant FIELD as a universal time."
  (let ((value (gethash field operation)))
    (and value
         (or (and (stringp value) (knowledge-parse-instant value))
             (memory-resource--fail "~A must be an ISO-8601 instant with a zone." field)))))

(-> memory-knowledge--assert (tool-context hash-table) string)
(defun memory-knowledge--assert (context operation)
  "Apply one fact-assert OPERATION, one support per source, and describe the result."
  (memory-knowledge--fields operation "fact-assert"
                            '("op" "predicate" "arguments" "negated" "sources" "basis"
                              "valid-from" "valid-until" "scope")
                            '("predicate" "arguments"))
  (let* ((configuration (tool-context-configuration context))
         (conversation (tool-context-conversation context))
         (predicate (gethash "predicate" operation))
         (schema (or (knowledge-base-schema (knowledge-base-read configuration) predicate
                                            :workspace (knowledge--workspace configuration))
                     (memory-resource--fail "Define predicate ~A with predicate-define first."
                                            predicate)))
         (proposition (knowledge-make-proposition
                       schema
                       (memory-knowledge--list (gethash "arguments" operation) "arguments")
                       :negated (and (gethash "negated" operation)
                                     (not (json-false-p (gethash "negated" operation))))))
         (now (get-universal-time))
         (requests (and (gethash "sources" operation)
                        (memory-resource--operation-sources operation)))
         (evidence (mapcar (lambda (request)
                             (memory-knowledge--evidence
                              context (memory-resource--observe-source context request now)))
                           requests))
         (named-basis (gethash "basis" operation))
         (basis (and named-basis
                     (or (find named-basis '(:observation :user-assertion :assumption
                                             :model-proposal)
                               :key (lambda (keyword) (string-downcase (symbol-name keyword)))
                               :test #'string=)
                         (memory-resource--fail "basis must be observation, user-assertion, assumption, or model-proposal."))))
         (claim nil))
    (if evidence
        (dolist (item evidence)
          (setf claim (knowledge-assert
                       configuration proposition
                       :scope (memory-knowledge--scope operation)
                       :evidence item
                       :basis (or basis
                                  (if (eq (first (getf (rest item) :source)) :user)
                                      ':user-assertion
                                      ':observation))
                       :valid-from (memory-knowledge--time operation "valid-from")
                       :valid-until (memory-knowledge--time operation "valid-until")
                       :source-conversation (conversation-identifier conversation))))
        (setf claim (knowledge-assert
                     configuration proposition
                     :scope (memory-knowledge--scope operation)
                     :basis (or basis ':model-proposal)
                     :valid-from (memory-knowledge--time operation "valid-from")
                     :valid-until (memory-knowledge--time operation "valid-until")
                     :source-conversation (conversation-identifier conversation))))
    (format nil "Asserted ~A: ~A."
            claim (knowledge-proposition-display proposition schema))))

(-> memory-knowledge--retract (tool-context hash-table) string)
(defun memory-knowledge--retract (context operation)
  "Apply one fact-retract OPERATION and describe the result."
  (memory-knowledge--fields operation "fact-retract" '("op" "id" "reason") '("id"))
  (let ((retracted (knowledge-retract (tool-context-configuration context)
                                      (gethash "id" operation)
                                      :reason (gethash "reason" operation)
                                      :source-conversation
                                      (conversation-identifier
                                       (tool-context-conversation context)))))
    (format nil "Retracted ~D support~:P of ~A." (length retracted) (gethash "id" operation))))

(-> memory-knowledge--propose (tool-context hash-table) string)
(defun memory-knowledge--propose (context operation)
  "Apply one rule-propose OPERATION and describe the result."
  (memory-knowledge--fields operation "rule-propose"
                            '("op" "id" "premises" "guards" "bindings" "conclusion"
                              "description" "scope")
                            '("id" "premises" "conclusion"))
  (multiple-value-bind (identifier version new-p)
      (knowledge-propose-rule (tool-context-configuration context)
                              :id (gethash "id" operation)
                              :premises (gethash "premises" operation)
                              :guards (gethash "guards" operation #())
                              :bindings (gethash "bindings" operation #())
                              :conclusion (gethash "conclusion" operation)
                              :description (gethash "description" operation)
                              :scope (memory-knowledge--scope operation)
                              :source-conversation (conversation-identifier
                                                    (tool-context-conversation context)))
    (format nil "~:[Rule ~A version ~D is unchanged~;Proposed rule ~A version ~D~]; it derives nothing until it is admitted."
            new-p identifier version)))

(-> memory-knowledge--apply (tool-context t) string)
(defun memory-knowledge--apply (context operation)
  "Apply one memory:facts OPERATION and return its one-line result."
  (unless (hash-table-p operation)
    (memory-resource--fail "Each operation is an object with an op field."))
  (let ((name (gethash "op" operation)))
    (cond
      ((equal name "predicate-define")
       (memory-knowledge--define (tool-context-configuration context) operation))
      ((equal name "fact-assert")
       (memory-knowledge--assert context operation))
      ((equal name "fact-retract")
       (memory-knowledge--retract context operation))
      ((equal name "rule-propose")
       (memory-knowledge--propose context operation))
      (t
       (memory-resource--fail "memory:facts accepts predicate-define, fact-assert, fact-retract, and rule-propose.")))))

(-> memory-knowledge--change-lines (list) list)
(defun memory-knowledge--change-lines (changes)
  "Return one line describing each knowledge change."
  (mapcar (lambda (change)
            (ecase (getf change :kind)
              (:new (format nil "now holds: ~A (~A)" (getf change :text) (getf change :claim)))
              (:withdrawn (format nil "no longer holds: ~A~@[ because ~{~A~^; ~}~]"
                                  (getf change :text) (getf change :causes)))
              (:changed (format nil "changed: ~A, was ~A" (getf change :to) (getf change :from)))
              (:conflict (format nil "conflicts: ~A (~A)" (getf change :text)
                                 (getf change :claim)))))
          changes))

(defmethod resource-tool-edit
    ((resource memory-knowledge-resource) (tool resource-edit-tool)
     (context tool-context) (arguments hash-table))
  "Apply up to the operation limit to memory:facts and report what now holds."
  (declare (ignore tool))
  (unless (eq (memory-knowledge-resource-view-kind resource) ':facts)
    (memory-resource--fail "Only memory:facts accepts edits."))
  (let* ((conversation (tool-context-conversation context))
         (base-revision (tool-argument arguments "base-revision" :required t))
         (operations (memory-knowledge--list (tool-argument arguments "operations" :required t)
                                             "operations")))
    (unless (<= 1 (length operations) *memory-knowledge-operation-limit*)
      (memory-resource--fail "memory:facts accepts 1 to ~D operations per edit."
                             *memory-knowledge-operation-limit*))
    ;; Additions do not depend on what was read, so any memory:facts revision
    ;; observed in this conversation authorizes an edit.
    (handler-case
        (resource-item-find-observation-state conversation resource base-revision)
      (resource-revision-stale ()
        (memory-resource--fail "Read memory:facts in this conversation and edit against a revision it returned.")))
    (let ((before (knowledge-view-snapshot (memory-knowledge-view context)))
          (results nil))
      (loop for operation in operations
            for index from 1
            do (handler-case (push (memory-knowledge--apply context operation) results)
                 ((or knowledge-error memory-error tool-error) (condition)
                   (memory-resource--fail "Operation ~D failed: ~A~@[ Operations 1 to ~D were applied.~]"
                                          index (autolith-error-message condition)
                                          (and (> index 1) (1- index))))))
      (let* ((changes (knowledge-view-changes before (memory-knowledge-view context)))
             (observation (memory-knowledge-observe resource context :maximum-results 1))
             (state (resource-observation-state-ensure conversation observation)))
        (tool-success
         (format nil "~{~A~%~}~:[~;~:*~{~A~%~}~]Revision: ~A"
                 (reverse results)
                 (memory-knowledge--change-lines changes)
                 (resource-observation-state-alias state)))))))
