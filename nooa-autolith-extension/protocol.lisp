(in-package #:autolith)

;;;; -- Domain Agent Protocol --

(define-condition domain-agent-error (autolith-error)
  ((agent
    :initarg :agent
    :initform nil
    :reader domain-agent-error-agent
    :type t
    :documentation "The domain agent involved in the failure, when known.")
   (capability
    :initarg :capability
    :initform nil
    :reader domain-agent-error-capability
    :type (option symbol)
    :documentation "The capability that could not run, when known.")
   (cause
    :initarg :cause
    :initform nil
    :reader domain-agent-error-cause
    :type t
    :documentation "The underlying condition, when this error wraps another."))
  (:documentation
   "A domain-agent definition, invocation, or validated return failed.")
  (:report
   (lambda (condition stream)
     (format stream "Domain agent~@[ ~A~]~@[ capability ~S~]: ~A"
             (let ((agent (domain-agent-error-agent condition)))
               (and agent (class-name (class-of agent))))
             (domain-agent-error-capability condition)
             (autolith-error-message condition)))))


(defclass domain-agent ()
  ((--lock
    :initform (make-recursive-lock "Autolith domain agent")
    :reader domain-agent--lock
    :documentation "The recursive lock serializing agentic invocations on this instance."))
  (:documentation
   "A CLOS object whose registered methods are its model-visible capabilities."))

(defclass domain-capability ()
  ((name
    :initarg :name
    :reader domain-capability-name
    :type symbol
    :documentation "The generic function implementing this capability.")
   (agent-class
    :initarg :agent-class
    :reader domain-capability-agent-class
    :type symbol
    :documentation "The domain-agent class this capability specializes on.")
   (parameter-names
    :initarg :parameter-names
    :reader domain-capability-parameter-names
    :type list
    :documentation "Required parameter names after the agent argument.")
   (lambda-list
    :initarg :lambda-list
    :reader domain-capability-lambda-list
    :type list
    :documentation "The specialized lambda list from the defining form.")
   (documentation
    :initarg :documentation
    :initform ""
    :reader domain-capability-documentation
    :type string
    :documentation "The model-visible description of this capability.")
   (kind
    :initarg :kind
    :reader domain-capability-kind
    :type (member :lisp :agentic)
    :documentation "Whether the method body is Lisp or an LLM loop.")
   (contract
    :initarg :contract
    :initform ':text
    :reader domain-capability-contract
    :type t
    :documentation "The native return contract for an agentic method.")
   (strategy
    :initarg :strategy
    :initform nil
    :reader domain-capability-strategy
    :type (option (member :predict :code-act))
    :documentation "The agentic execution strategy, or NIL for Lisp methods.")
   (budget-arguments
    :initarg :budget-arguments
    :initform nil
    :reader domain-capability-budget-arguments
    :type list
    :documentation "Default RLM-BUDGET-CREATE keyword arguments."))
  (:documentation "One registered domain-agent capability."))


(defvar *domain-capabilities* (make-hash-table :test #'equal)
  "Maps (NAME AGENT-CLASS) conses to registered DOMAIN-CAPABILITY objects.")

(defvar *domain-capability-lock* (make-lock "Autolith domain capabilities")
  "The lock serializing domain-capability registration.")

(defvar *domain-agent-provider* nil
  "The provider used by agentic methods in the current dynamic extent.")

(defvar *domain-agent-configuration* nil
  "The configuration used by agentic methods in the current dynamic extent.")

(defvar *domain-agent-budget* nil
  "The RLM budget used by agentic methods in the current dynamic extent.")


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *domain-agentic-option-keys*
    '(:documentation :contract :strategy :budget)
    "The literal option keys accepted by DEFINE-AGENTIC-METHOD.")

  (defun domain-agent--proper-list-p (value)
    "Return true when VALUE is a finite proper list."
    (and (listp value)
         (integerp (list-length value))))

  (defun domain-agent--parameter-name (entry)
    "Return the variable name of specialized or unspecialized ENTRY."
    (cond
      ((and (symbolp entry)
            (not (member entry lambda-list-keywords :test #'eq)))
       entry)
      ((and (consp entry)
            (symbolp (first entry))
            (or (null (rest entry))
                (and (symbolp (second entry))
                     (null (rest (rest entry))))))
       (first entry))
      (t
       (error 'domain-agent-error
              :message
              (format nil
                      "Domain-agent lambda lists use required parameters, not ~S."
                      entry)))))

  (defun domain-agent--required-lambda-list-p (lambda-list)
    "Return true when LAMBDA-LIST is a specialized required-parameter list."
    (and (domain-agent--proper-list-p lambda-list)
         (not (null lambda-list))
         (every (lambda (entry)
                  (not (member entry lambda-list-keywords :test #'eq)))
                lambda-list)))

  (defun domain-agent--agent-specializer (lambda-list)
    "Return the class name specializing LAMBDA-LIST's first parameter."
    (let ((entry (first lambda-list)))
      (unless (and (consp entry)
                   (symbolp (first entry))
                   (symbolp (second entry))
                   (null (rest (rest entry))))
        (error 'domain-agent-error
               :message
               "The first domain-agent parameter must be (AGENT CLASS)."))
      (second entry)))

  (defun domain-agent--parse-agentic-options (options)
    "Return a plist of validated DEFINE-AGENTIC-METHOD OPTIONS.

Each option is a two-element list (KEY VALUE)."
    (unless (and (domain-agent--proper-list-p options)
                 (every (lambda (option)
                          (and (domain-agent--proper-list-p option)
                               (= (length option) 2)
                               (member (first option)
                                       *domain-agentic-option-keys*
                                       :test #'eq)))
                        options)
                 (= (length options)
                    (length (remove-duplicates options :key #'first))))
      (error 'domain-agent-error
             :message
             "DEFINE-AGENTIC-METHOD options are unique (KEY VALUE) lists of :DOCUMENTATION, :CONTRACT, :STRATEGY, and :BUDGET."))
    (let ((documentation "")
          (contract ':text)
          (strategy ':predict)
          (budget nil))
      (dolist (option options)
        (ecase (first option)
          (:documentation
           (setf documentation (second option)))
          (:contract
           (setf contract (second option)))
          (:strategy
           (setf strategy (second option)))
          (:budget
           (setf budget (second option)))))
      (unless (stringp documentation)
        (error 'domain-agent-error
               :message "DEFINE-AGENTIC-METHOD :DOCUMENTATION must be a string."))
      (unless (member strategy '(:predict :code-act) :test #'eq)
        (error 'domain-agent-error
               :message
               "DEFINE-AGENTIC-METHOD :STRATEGY must be :PREDICT or :CODE-ACT."))
      (unless (or (null budget) (domain-agent--proper-list-p budget))
        (error 'domain-agent-error
               :message
               "DEFINE-AGENTIC-METHOD :BUDGET must be a keyword argument list."))
      (list :documentation documentation
            :contract contract
            :strategy strategy
            :budget budget))))


(-> domain-agent--capability-key (symbol symbol) list)
(defun domain-agent--capability-key (name agent-class)
  "Return the registry key for NAME specialized on AGENT-CLASS."
  (list name agent-class))

(-> domain-agent--register (domain-capability) domain-capability)
(defun domain-agent--register (capability)
  "Install CAPABILITY, replacing any previous registration of the same key."
  (let ((class-name (domain-capability-agent-class capability)))
    (let ((class (find-class class-name nil)))
      (unless (and class (subtypep class 'domain-agent))
        (error 'domain-agent-error
               :capability (domain-capability-name capability)
               :message
               (format nil "~S is not a DOMAIN-AGENT class." class-name))))
    (when (eq (domain-capability-kind capability) ':agentic)
      (rlm-contract-normalize (domain-capability-contract capability)))
    (with-lock-held (*domain-capability-lock*)
      (setf (gethash (domain-agent--capability-key
                      (domain-capability-name capability)
                      class-name)
                     *domain-capabilities*)
            capability)))
  capability)

(-> domain-agent--find (t symbol) (option domain-capability))
(defun domain-agent--find (agent-class name)
  "Return the most specific registered CAPABILITY named NAME for AGENT-CLASS."
  (closer-mop:finalize-inheritance agent-class)
  (with-lock-held (*domain-capability-lock*)
    (dolist (candidate (closer-mop:class-precedence-list agent-class))
      (let ((capability
              (gethash (domain-agent--capability-key name (class-name candidate))
                       *domain-capabilities*)))
        (when capability
          (return capability))))))

(-> domain-agent-capabilities (domain-agent) list)
(defun domain-agent-capabilities (agent)
  "Return the visible capabilities applicable to AGENT, most-specific first."
  (let ((class (class-of agent))
        (seen (make-hash-table :test #'eq))
        (capabilities '()))
    (closer-mop:finalize-inheritance class)
    (with-lock-held (*domain-capability-lock*)
      (dolist (candidate (closer-mop:class-precedence-list class))
        (maphash
         (lambda (key capability)
           (when (and (eq (second key) (class-name candidate))
                      (not (gethash (first key) seen)))
             (setf (gethash (first key) seen) t)
             (push capability capabilities)))
         *domain-capabilities*)))
    (nreverse capabilities)))

(-> domain-agent--default-budget (domain-capability) rlm-budget)
(defun domain-agent--default-budget (capability)
  "Return a fresh budget for CAPABILITY's default arguments."
  (apply #'rlm-budget-create (domain-capability-budget-arguments capability)))

(defmacro with-domain-agent-environment
    ((&key provider configuration budget) &body body)
  "Bind the agentic-method provider environment around BODY.

PROVIDER, CONFIGURATION, and BUDGET are evaluated. A null argument
leaves the corresponding special variable unchanged."
  (let ((provider-name (gensym "PROVIDER"))
        (configuration-name (gensym "CONFIGURATION"))
        (budget-name (gensym "BUDGET")))
    `(let ((,provider-name ,provider)
           (,configuration-name ,configuration)
           (,budget-name ,budget))
       (let ((*domain-agent-provider*
               (or ,provider-name *domain-agent-provider*))
             (*domain-agent-configuration*
               (or ,configuration-name *domain-agent-configuration*))
             (*domain-agent-budget*
               (or ,budget-name *domain-agent-budget*)))
         ,@body))))

(-> domain-agent-document (domain-agent) string)
(defun domain-agent-document (agent)
  "Return the model-visible catalog of AGENT's class, slots, and capabilities."
  (let ((class (class-of agent)))
    (closer-mop:finalize-inheritance class)
    (with-output-to-string (stream)
      (format stream "Class: ~A~%" (class-name class))
      (let ((documentation (documentation class t)))
        (when (non-empty-string-p documentation)
          (format stream "~A~%~%" documentation)))
      (format stream "Slots:~%")
      (let ((printed-p nil))
        (dolist (slot (class-slots class))
          (let ((name (slot-definition-name slot)))
            (when (and (not (value-preview-internal-slot-name-p name))
                       (slot-boundp agent name))
              (setf printed-p t)
              (format stream "  ~A: ~A~%"
                      (symbol-name name)
                      (value-preview (slot-value agent name))))))
        (unless printed-p
          (format stream "  none~%")))
      (format stream "~%Capabilities:~%")
      (let ((capabilities (domain-agent-capabilities agent)))
        (if capabilities
            (dolist (capability capabilities)
              (format stream "  ~A (~{~A~^ ~}) [~A~@[ ~A~]]~%~@[    ~A~%~]"
                      (domain-capability-name capability)
                      (domain-capability-parameter-names capability)
                      (domain-capability-kind capability)
                      (domain-capability-strategy capability)
                      (let ((documentation
                              (domain-capability-documentation capability)))
                        (and (non-empty-string-p documentation)
                             documentation))))
            (format stream "  none~%"))))))

(-> domain-agent--argument-views (list list) list)
(defun domain-agent--argument-views (names values)
  "Return labeled preview views for NAMES bound to live VALUES."
  (mapcar
   (lambda (name value)
     (list ':label (symbol-name name)
           ':content (value-preview value)))
   names values))

(-> domain-agent--resolve-environment
    (&key (:provider (option model-provider))
          (:configuration (option configuration))
          (:budget (option rlm-budget))
          (:capability (option domain-capability)))
    (values model-provider configuration rlm-budget))
(defun domain-agent--resolve-environment
    (&key provider configuration budget capability)
  "Return the provider, configuration, and budget for one agentic call."
  (let ((provider (or provider *domain-agent-provider*))
        (configuration (or configuration *domain-agent-configuration*))
        (budget (or budget *domain-agent-budget*
                    (and capability
                         (domain-agent--default-budget capability)))))
    (unless (and provider configuration)
      (handler-case
          (multiple-value-bind (environment-provider environment-configuration)
              (rlm--environment)
            (setf provider (or provider environment-provider)
                  configuration (or configuration environment-configuration)))
        (rlm-inference-error (condition)
          (error 'domain-agent-error
                 :capability (and capability
                                  (domain-capability-name capability))
                 :cause condition
                 :message (rlm-inference-error-message condition)))))
    (values provider configuration
            (or budget (rlm-budget-create)))))

(declaim (ftype (function (domain-agent domain-capability list
                                         &key
                                         (:provider model-provider)
                                         (:configuration configuration)
                                         (:budget rlm-budget))
                          t)
                domain-agent--predict
                domain-agent--code-act))

(-> domain-agent-invoke
    (domain-agent symbol list
     &key (:provider (option model-provider))
          (:configuration (option configuration))
          (:budget (option rlm-budget)))
    t)
(defun domain-agent-invoke
    (agent name arguments &key provider configuration budget)
  "Run AGENT's agentic capability NAME on ARGUMENTS and return its value.

PROVIDER, CONFIGURATION, and BUDGET default to the dynamic domain-agent
environment, then to the active application. The return value satisfies
the capability's contract or a condition is signaled."
  (let ((capability (domain-agent--find (class-of agent) name)))
    (unless capability
      (error 'domain-agent-error
             :agent agent
             :capability name
             :message
             (format nil "No registered capability named ~S." name)))
    (unless (eq (domain-capability-kind capability) ':agentic)
      (error 'domain-agent-error
             :agent agent
             :capability name
             :message
             (format nil "~S is a Lisp capability; call it as a method." name)))
    (unless (= (length arguments)
               (length (domain-capability-parameter-names capability)))
      (error 'domain-agent-error
             :agent agent
             :capability name
             :message
             (format nil "~S expected ~D argument~:P, not ~D."
                     name
                     (length (domain-capability-parameter-names capability))
                     (length arguments))))
    (with-recursive-lock-held ((domain-agent--lock agent))
      (multiple-value-bind (provider configuration budget)
          (domain-agent--resolve-environment
           :provider provider
           :configuration configuration
           :budget budget
           :capability capability)
        (with-domain-agent-environment
            (:provider provider :configuration configuration :budget budget)
          (ecase (domain-capability-strategy capability)
            (:predict
             (domain-agent--predict agent capability arguments
                                    :provider provider
                                    :configuration configuration
                                    :budget budget))
            (:code-act
             (domain-agent--code-act agent capability arguments
                                     :provider provider
                                     :configuration configuration
                                     :budget budget))))))))

(defmacro define-domain-capability (name lambda-list &body body)
  "Define NAME as a deterministic, model-visible method on a domain agent.

LAMBDA-LIST is a specialized required-parameter list whose first
parameter is (AGENT CLASS). The optional documentation string is the
model-visible description. BODY is ordinary Common Lisp."
  (unless (and (symbolp name)
               (domain-agent--required-lambda-list-p lambda-list))
    (error 'domain-agent-error
           :capability (and (symbolp name) name)
           :message
           "DEFINE-DOMAIN-CAPABILITY requires a symbol name and a required specialized lambda list."))
  (let* ((agent-class (domain-agent--agent-specializer lambda-list))
         (parameter-names (mapcar #'domain-agent--parameter-name lambda-list))
         (documentation (and (stringp (first body)) (first body)))
         (forms (if documentation (rest body) body)))
    `(progn
       (defmethod ,name ,lambda-list
         ,@(when documentation (list documentation))
         ,@forms)
       (eval-when (:load-toplevel :execute)
         (domain-agent--register
          (make-instance 'domain-capability
                         :name ',name
                         :agent-class ',agent-class
                         :parameter-names ',(rest parameter-names)
                         :lambda-list ',lambda-list
                         :documentation ,(or documentation "")
                         :kind ':lisp)))
       ',name)))

(defmacro define-agentic-method (name lambda-list &rest options)
  "Define NAME as an LLM-implemented method on a domain agent.

LAMBDA-LIST is a specialized required-parameter list whose first
parameter is (AGENT CLASS). OPTIONS are unique (KEY VALUE) lists of
:DOCUMENTATION, :CONTRACT, :STRATEGY, and :BUDGET. :STRATEGY is
:PREDICT or :CODE-ACT.
:CONTRACT reuses the native task output schema language, or :TEXT.
Calling the generic function runs the strategy and returns a value that
has already passed the contract."
  (unless (and (symbolp name)
               (domain-agent--required-lambda-list-p lambda-list))
    (error 'domain-agent-error
           :capability (and (symbolp name) name)
           :message
           "DEFINE-AGENTIC-METHOD requires a symbol name and a required specialized lambda list."))
  (let* ((parsed (domain-agent--parse-agentic-options options))
         (agent-class (domain-agent--agent-specializer lambda-list))
         (parameter-names (mapcar #'domain-agent--parameter-name lambda-list))
         (agent-variable (first parameter-names))
         (argument-names (rest parameter-names))
         (documentation (getf parsed ':documentation))
         (contract (getf parsed ':contract))
         (strategy (getf parsed ':strategy))
         (budget (getf parsed ':budget)))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (when (fboundp ',name)
           (unless (typep (fdefinition ',name) 'generic-function)
             (error 'domain-agent-error
                    :capability ',name
                    :message
                    (format nil "~S is fbound and is not a generic function."
                            ',name))))
         (unless (fboundp ',name)
           (defgeneric ,name ,parameter-names
             (:documentation ,documentation))))
       (defmethod ,name ,lambda-list
         ,@(when (non-empty-string-p documentation)
             (list documentation))
         (domain-agent-invoke ,agent-variable ',name
                              (list ,@argument-names)))
       (eval-when (:load-toplevel :execute)
         (domain-agent--register
          (make-instance 'domain-capability
                         :name ',name
                         :agent-class ',agent-class
                         :parameter-names ',argument-names
                         :lambda-list ',lambda-list
                         :documentation ,documentation
                         :kind ':agentic
                         :contract ',contract
                         :strategy ',strategy
                         :budget-arguments ',budget)))
       ',name)))
