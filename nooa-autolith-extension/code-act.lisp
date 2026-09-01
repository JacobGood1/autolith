(in-package #:autolith)

;;;; -- CodeAct Strategy --

(defparameter *domain-agent-code-act-instructions*
  "You implement one domain-agent method by writing exactly one Lisp form
per turn. The form is interpreted, not evaluated in the Autolith image.

Allowed forms:
  (preview NAME)
      Show a bounded preview of a live binding.
  (call CAPABILITY ARG*)
      Invoke a registered capability. CAPABILITY is a symbol. Each ARG is
      either a live binding name or literal data. The result is bound to
      *LAST* and to a fresh VN name.
  (finish VALUE)
      End the method. VALUE is a live binding name or literal data that
      must satisfy the method contract.

Symbols other than live bindings are data. Do not write DEFUN, EVAL,
LOAD, or any form outside those three operators. Reply with one form."
  "The standing CodeAct instructions shown to the model.")

(defclass domain-agent-session ()
  ((agent
    :initarg       :agent
    :reader        domain-agent-session-agent
    :type          domain-agent
    :documentation "The live domain agent this session operates on.")
   (capability
    :initarg       :capability
    :reader        domain-agent-session-capability
    :type          domain-capability
    :documentation "The agentic method this session implements.")
   (environment
    :initarg       :environment
    :reader        domain-agent-session-environment
    :type          hash-table
    :documentation "The live name-to-object bindings visible to generated forms.")
   (next-result
    :initform      1
    :accessor      domain-agent-session-next-result
    :type          (integer 1)
    :documentation "The next VN binding index.")
   (observations
    :initform      nil
    :accessor      domain-agent-session-observations
    :type          list
    :documentation "Oldest-first interpreter observations for later turns."))
  (:documentation "One CodeAct session over live domain-agent bindings."))

(-> domain-agent--binding-symbol (string) symbol)
(defun domain-agent--binding-symbol (name)
  "Return NAME interned in #:AUTOLITH for session bindings."
  (intern (string-upcase name) (find-package '#:autolith)))

(-> domain-agent--session
    (domain-agent domain-capability list)
    domain-agent-session)
(defun domain-agent--session (agent capability arguments)
  "Return a session binding AGENT, SELF, and CAPABILITY's live arguments."
  (let ((environment (make-hash-table :test #'eq)))
    (setf (gethash (domain-agent--binding-symbol "SELF") environment) agent
          (gethash (domain-agent--binding-symbol "*LAST*") environment) nil)
    (mapc (lambda (name value)
            (setf (gethash name environment) value))
          (domain-capability-parameter-names capability)
          arguments)
    (make-instance 'domain-agent-session
                   :agent       agent
                   :capability  capability
                   :environment environment)))

(-> domain-agent--bound-p (domain-agent-session symbol) boolean)
(defun domain-agent--bound-p (session name)
  "Return true when NAME is a live binding in SESSION."
  (nth-value 1 (gethash name (domain-agent-session-environment session))))

(-> domain-agent--binding (domain-agent-session symbol) t)
(defun domain-agent--binding (session name)
  "Return SESSION's live object named NAME."
  (multiple-value-bind (value present-p)
      (gethash name (domain-agent-session-environment session))
    (unless present-p
      (error 'domain-agent-error
             :agent      (domain-agent-session-agent session)
             :capability (domain-capability-name
                          (domain-agent-session-capability session))
             :message    (format nil "No live binding named ~S." name)))
    value))

(-> domain-agent--bind (domain-agent-session symbol t) symbol)
(defun domain-agent--bind (session name value)
  "Store VALUE under NAME in SESSION and return NAME."
  (setf (gethash name (domain-agent-session-environment session)) value)
  name)

(-> domain-agent--resolve (domain-agent-session t) t)
(defun domain-agent--resolve (session value)
  "Return VALUE with live binding names replaced by their objects."
  (cond
    ((and (symbolp value)
          (not (keywordp value))
          (domain-agent--bound-p session value))
     (domain-agent--binding session value))
    ((consp value)
     (cons (domain-agent--resolve session (first value))
           (domain-agent--resolve session (rest value))))
    (t
     value)))

(-> domain-agent--record (domain-agent-session string) null)
(defun domain-agent--record (session observation)
  "Append OBSERVATION to SESSION's chronological notes."
  (setf (domain-agent-session-observations session)
        (append (domain-agent-session-observations session)
                (list observation)))
  nil)

(-> domain-agent--store-result (domain-agent-session t) string)
(defun domain-agent--store-result (session value)
  "Bind VALUE as *LAST* and a fresh VN name, and return the observation."
  (let ((name (domain-agent--binding-symbol
               (format nil "V~D"
                       (domain-agent-session-next-result session)))))
    (incf (domain-agent-session-next-result session))
    (domain-agent--bind session name value)
    (domain-agent--bind session (domain-agent--binding-symbol "*LAST*") value)
    (format nil "~A and *LAST* bound to ~A" name (value-preview value))))

(-> domain-agent--contract-json (t t) (values t boolean string))
(defun domain-agent--contract-json (contract value)
  "Return JSON for VALUE, whether it satisfies CONTRACT, and a problem."
  (if (eq contract ':text)
      (if (stringp value)
          (values value t "")
          (values nil nil "A :TEXT contract requires a string."))
      (handler-case
          (let ((json (if (or (json-object-p value)
                              (vectorp value)
                              (stringp value)
                              (numberp value)
                              (eq value t)
                              (json-false-p value)
                              (eq value ':null))
                          value
                          (task-sexp->json value))))
            (if (task-output-schema-valid-p json (rlm-contract-normalize contract))
                (values (task-json->sexp json) t "")
                (values nil nil
                        "The finished value does not satisfy the method contract.")))
        (error (condition)
          (values nil nil
                  (format nil "The finished value is not portable contract data: ~A"
                          condition))))))

(-> domain-agent--extract-form (string) t)
(defun domain-agent--extract-form (text)
  "Read exactly one Lisp form from TEXT with reader evaluation disabled."
  (let* ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) text))
         (unfenced
           (let ((start (search "```" trimmed)))
             (if start
                 (let* ((after   (subseq trimmed (+ start 3)))
                        (newline (position #\Newline after))
                        (body    (if newline (subseq after (1+ newline)) after))
                        (stop    (search "```" body)))
                   (string-trim '(#\Space #\Tab #\Newline #\Return)
                                (if stop (subseq body 0 stop) body)))
                 trimmed)))
         (start   (position #\( unfenced)))
    (unless start
      (error 'domain-agent-error
             :message "The model reply did not contain a Lisp form."))
    (let* ((source      (subseq unfenced start))
           (*read-eval* nil)
           (*package*   (find-package '#:autolith)))
      (with-standard-io-syntax
        (let ((*read-eval* nil)
              (*package*   (find-package '#:autolith)))
          (multiple-value-bind (form position)
              (read-from-string source)
            (let ((rest (string-trim '(#\Space #\Tab #\Newline #\Return)
                                     (subseq source position))))
              (unless (string= rest "")
                (error 'domain-agent-error
                       :message "The model reply contained more than one Lisp form."))
              form)))))))

(-> domain-agent--operator-name (t) (option string))
(defun domain-agent--operator-name (form)
  "Return the uppercase operator name of FORM, or NIL."
  (and (consp form)
       (symbolp (first form))
       (string-upcase (symbol-name (first form)))))

(-> domain-agent--call-capability
    (domain-agent-session symbol list)
    t)
(defun domain-agent--call-capability (session name arguments)
  "Apply registered capability NAME to resolved ARGUMENTS in SESSION."
  (let* ((agent      (domain-agent-session-agent session))
         (capability (domain-agent--find (class-of agent) name)))
    (unless capability
      (error 'domain-agent-error
             :agent      agent
             :capability name
             :message    (format nil "No registered capability named ~S." name)))
    (let ((resolved (mapcar (lambda (argument)
                              (domain-agent--resolve session argument))
                            arguments)))
      (unless (= (length resolved)
                 (length (domain-capability-parameter-names capability)))
        (error 'domain-agent-error
               :agent      agent
               :capability name
               :message
               (format nil "~S expected ~D argument~:P, not ~D."
                       name
                       (length (domain-capability-parameter-names capability))
                       (length resolved))))
      (if (eq (domain-capability-kind capability) ':agentic)
          (domain-agent-invoke
           agent name resolved
           :budget
           (rlm-budget-descend *domain-agent-budget*
                               :task (symbol-name name)))
          (apply (fdefinition name) agent resolved)))))

(-> domain-agent--interpret (domain-agent-session t)
    (values t boolean))
(defun domain-agent--interpret (session form)
  "Interpret FORM against SESSION.

The second value is true when FORM successfully finished the method."
  (let ((operator   (domain-agent--operator-name form))
        (capability (domain-agent-session-capability session)))
    (cond
      ((string= operator "PREVIEW")
       (unless (= (length (rest form)) 1)
         (error 'domain-agent-error
                :message "PREVIEW takes exactly one binding name."))
       (let ((name (second form)))
         (unless (symbolp name)
           (error 'domain-agent-error
                  :message "PREVIEW requires a symbol."))
         (domain-agent--record
          session
          (format nil "PREVIEW ~S => ~A"
                  name
                  (value-preview (domain-agent--binding session name)))))
       (values nil nil))
      ((string= operator "CALL")
       (unless (symbolp (second form))
         (error 'domain-agent-error
                :message "CALL requires a capability symbol."))
       (domain-agent--record
        session
        (domain-agent--store-result
         session
         (domain-agent--call-capability session (second form) (rest (rest form)))))
       (values nil nil))
      ((string= operator "FINISH")
       (unless (= (length (rest form)) 1)
         (error 'domain-agent-error
                :message "FINISH takes exactly one value."))
       (multiple-value-bind (value valid-p problem)
           (domain-agent--contract-json
            (domain-capability-contract capability)
            (domain-agent--resolve session (second form)))
         (unless valid-p
           (error 'domain-agent-error
                  :agent      (domain-agent-session-agent session)
                  :capability (domain-capability-name capability)
                  :message    problem))
         (domain-agent--record
          session
          (format nil "FINISH accepted ~A" (value-preview value)))
         (values value t)))
      (t
       (error 'domain-agent-error
              :message
              (format nil
                      "Unsupported form ~S. Use PREVIEW, CALL, or FINISH."
                      form))))))

(-> domain-agent--code-act-task (domain-agent-session) string)
(defun domain-agent--code-act-task (session)
  "Return the governing CodeAct task for SESSION's current turn."
  (let ((capability (domain-agent-session-capability session))
        (agent      (domain-agent-session-agent session)))
    (format nil
            "~A

Method: ~A
Documentation:
~A

Contract:
~A

Agent catalog:
~A

Live bindings:
~{~A~%~}"
            *domain-agent-code-act-instructions*
            (domain-capability-name capability)
            (let ((documentation (domain-capability-documentation capability)))
              (if (non-empty-string-p documentation)
                  documentation
                  "No additional documentation."))
            (if (eq (domain-capability-contract capability) ':text)
                ":TEXT"
                (json-encode
                 (task-output-schema->json
                  (rlm-contract-normalize
                   (domain-capability-contract capability)))))
            (domain-agent-document agent)
            (let ((lines '()))
              (maphash
               (lambda (name value)
                 (push (format nil "  ~S: ~A" name (value-preview value))
                       lines))
               (domain-agent-session-environment session))
              (sort lines #'string<)))))

(-> domain-agent--code-act
    (domain-agent domain-capability list
     &key (:provider model-provider)
          (:configuration configuration)
          (:budget rlm-budget))
    t)
(defun domain-agent--code-act
    (agent capability arguments &key provider configuration budget)
  "Run CAPABILITY as an interpreted Lisp CodeAct loop over live objects."
  (let ((session  (domain-agent--session agent capability arguments))
        (contract ':text))
    (loop
      (let ((request
              (format nil "~A~@[~%~%Previous observations:~%~{~A~%~}~]~%~A"
                      (domain-agent--code-act-task session)
                      (domain-agent-session-observations session)
                      "Reply with exactly one allowed Lisp form.")))
        (multiple-value-bind (text trace-identifier)
            (infer request
                   :contract      contract
                   :budget        budget
                   :provider      provider
                   :configuration configuration)
          (declare (ignore trace-identifier))
          (handler-case
              (multiple-value-bind (value finished-p)
                  (domain-agent--interpret
                   session
                   (domain-agent--extract-form text))
                (when finished-p
                  (return value)))
            (domain-agent-error (condition)
              (domain-agent--record
               session
               (format nil "Error: ~A" (autolith-error-message condition))))))))))
