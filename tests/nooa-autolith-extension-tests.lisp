(in-package #:autolith)

;;;; -- Domain Agent Extension Tests --

(defclass domain-agent-test-order ()
  ((delivered-p
    :initarg :delivered-p
    :reader domain-agent-test-order-delivered-p
    :type boolean
    :documentation "Whether the order has been delivered.")
   (days
    :initarg :days
    :reader domain-agent-test-order-days
    :type integer
    :documentation "Whole days since delivery.")
   (--secret
    :initform "hidden"
    :reader domain-agent-test-order-secret
    :documentation "Implementation state omitted from previews."))
  (:documentation "An order used by domain-agent contract tests."))

(defclass domain-agent-test-support (domain-agent)
  ((desk
    :initarg :desk
    :initform "returns"
    :accessor domain-agent-test-support-desk
    :type string
    :documentation "The support desk handling this agent."))
  (:documentation "A support specialist used by domain-agent contract tests."))

(define-domain-capability domain-agent-test-eligible-p
    ((agent domain-agent-test-support) (order domain-agent-test-order))
  "Return true when ORDER was delivered within 30 days."
  (declare (ignore agent))
  (and (domain-agent-test-order-delivered-p order)
       (<= (domain-agent-test-order-days order) 30)
       t))

(define-agentic-method domain-agent-test-classify
    ((agent domain-agent-test-support) text)
  (:documentation "Classify TEXT as spam or ham.")
  (:contract (:type :object
              :properties (("label" (:type :string)))
              :required ("label")))
  (:strategy :predict)
  (:budget (:calls 4 :tokens 1000 :depth 1)))

(define-agentic-method domain-agent-test-triage
    ((agent domain-agent-test-support) (order domain-agent-test-order))
  (:documentation "Decide whether ORDER is refund eligible.")
  (:contract (:type :object
              :properties (("eligible" (:type :boolean)))
              :required ("eligible")))
  (:strategy :code-act)
  (:budget (:calls 8 :tokens 4000 :depth 2)))


(-> test-value-preview () null)
(defun test-value-preview ()
  "Test bounded previews of strings, sequences, objects, and circularity."
  (test-assert (string= (value-preview nil) "NIL")
               "NIL previews as NIL")
  (test-assert (string= (value-preview t) "T")
               "T previews as T")
  (test-assert (string= (value-preview 42) "42")
               "integers preview with prin1")
  (test-assert (string= (value-preview "hi") "STRING 2 \"hi\"")
               "short strings preview in full")
  (let ((text (make-string 80 :initial-element #\a)))
    (test-assert
     (and (search "STRING 80" (value-preview text))
          (search "..." (value-preview text)))
     "long strings show length, head, and tail"))
  (test-assert (string= (value-preview '(1 2 3)) "LIST 3 (1 2 3)")
               "short lists preview in full")
  (test-assert
   (and (search "LIST 20" (value-preview (loop for index from 0 below 20
                                               collect index)))
        (search "..." (value-preview (loop for index from 0 below 20
                                           collect index))))
   "long lists show length, head, and tail")
  (test-assert (string= (value-preview #(1 2)) "VECTOR 2 (1 2)")
               "short vectors preview in full")
  (let ((order (make-instance 'domain-agent-test-order
                              :delivered-p t
                              :days 4)))
    (test-assert
     (and (search "DOMAIN-AGENT-TEST-ORDER" (value-preview order))
          (search "DELIVERED-P=T" (value-preview order))
          (not (search "hidden" (value-preview order)))
          (not (search "SECRET" (value-preview order))))
     "standard objects hide double-hyphen slots"))
  (let ((circular (cons nil nil)))
    (setf (first circular) circular)
    (test-assert (search "#CIRCULAR" (value-preview circular))
                 "circular structure is reported instead of looping"))
  (let ((preview
          (let ((*value-preview-character-limit* 20))
            (value-preview (make-string 80 :initial-element #\x)))))
    (test-assert (and (<= (length preview) 80)
                      (search "omitted" preview))
                 "previews clip to the character limit"))
  nil)

(-> test-domain-agent-catalog () null)
(defun test-domain-agent-catalog ()
  "Test domain-agent catalogs, capability lookup, and hidden locks."
  (let ((agent (make-instance 'domain-agent-test-support))
        (order (make-instance 'domain-agent-test-order
                              :delivered-p t
                              :days 2)))
    (test-assert (domain-agent-test-eligible-p agent order)
                 "deterministic capabilities run as ordinary methods")
    (test-assert
     (not (domain-agent-test-eligible-p
           agent
           (make-instance 'domain-agent-test-order
                          :delivered-p t
                          :days 40)))
     "deterministic capabilities return exact booleans")
    (let ((catalog (domain-agent-document agent)))
      (test-assert
       (and (search "DOMAIN-AGENT-TEST-SUPPORT" catalog)
            (search "A support specialist" catalog)
            (search "DESK:" catalog)
            (search "DOMAIN-AGENT-TEST-ELIGIBLE-P" catalog)
            (search "DOMAIN-AGENT-TEST-CLASSIFY" catalog)
            (search "DOMAIN-AGENT-TEST-TRIAGE" catalog)
            (search "[LISP]" catalog)
            (search "[AGENTIC PREDICT]" catalog)
            (search "[AGENTIC CODE-ACT]" catalog)
            (not (search "--LOCK" catalog)))
       "the catalog lists public slots and capabilities and hides the lock"))
    (test-assert
     (eq (domain-capability-strategy
          (domain-agent--find (class-of agent) 'domain-agent-test-classify))
         ':predict)
     "capability lookup finds the most specific agentic method")
    (test-assert
     (handler-case
         (progn
           (macroexpand-1
            '(define-agentic-method domain-agent-test-bad
              ((agent domain-agent-test-support) text)
              (:strategy :graph)))
           nil)
       (domain-agent-error ()
         t))
     "DEFINE-AGENTIC-METHOD rejects unknown strategies"))
  nil)

(-> test-domain-agent-predict () null)
(defun test-domain-agent-predict ()
  "Test Predict repairs invalid JSON then returns a validated value."
  (let ((configuration (test-configuration))
        (agent (make-instance 'domain-agent-test-support))
        (provider
          (make-instance
           'rlm-inference-test-provider
           :results
           (list (rlm-inference-test-result "resp-1" "not json" 20)
                 (rlm-inference-test-result
                  "resp-2" "{\"label\": \"spam\"}" 30))))
        (budget (rlm-budget-create :calls 4 :tokens 1000 :depth 1)))
    (unwind-protect
         (with-domain-agent-environment
             (:provider provider :configuration configuration :budget budget)
           (let ((value (domain-agent-test-classify agent "buy now")))
             (test-assert (equal value '(:object ("label" "spam")))
                          "Predict returns portable tagged contract data")
             (test-assert (= (rlm-budget-remaining-calls budget) 2)
                          "Predict charges the repair round")))
      (uiop:delete-directory-tree
       (test-configuration-root configuration)
       :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-domain-agent-code-act () null)
(defun test-domain-agent-code-act ()
  "Test CodeAct call/finish, invalid termination, and rejected eval forms."
  (let ((configuration (test-configuration))
        (agent (make-instance 'domain-agent-test-support))
        (order (make-instance 'domain-agent-test-order
                              :delivered-p t
                              :days 5))
        (provider
          (make-instance
           'rlm-inference-test-provider
           :results
           (list (rlm-inference-test-result
                  "resp-1" "(call domain-agent-test-eligible-p order)" 20)
                 (rlm-inference-test-result
                  "resp-2" "(finish \"nope\")" 20)
                 (rlm-inference-test-result
                  "resp-3" "(eval '(+ 1 2))" 20)
                 (rlm-inference-test-result
                  "resp-4"
                  "(finish (:object (\"eligible\" *LAST*)))"
                  40))))
        (budget (rlm-budget-create :calls 8 :tokens 4000 :depth 2)))
    (unwind-protect
         (with-domain-agent-environment
             (:provider provider :configuration configuration :budget budget)
           (let ((value (domain-agent-test-triage agent order)))
             (test-assert (equal value '(:object ("eligible" t)))
                          "CodeAct finish validates against the contract")
             (test-assert (= (rlm-budget-remaining-calls budget) 4)
                          "CodeAct charges every interpreted turn")
             (test-assert
              (null (rlm-inference-test-provider-results provider))
              "CodeAct consumed the invalid finish and rejected eval form")))
      (uiop:delete-directory-tree
       (test-configuration-root configuration)
       :validate t :if-does-not-exist ':ignore)))
  (let ((configuration (test-configuration))
        (agent (make-instance 'domain-agent-test-support))
        (order (make-instance 'domain-agent-test-order
                              :delivered-p nil
                              :days 1))
        (provider
          (make-instance
           'rlm-inference-test-provider
           :results
           (list (rlm-inference-test-result
                  "resp-1"
                  (format nil "```lisp~%(preview order)~%```")
                  10)
                 (rlm-inference-test-result
                  "resp-2"
                  "(finish (:object (\"eligible\" nil)))"
                  10))))
        (budget (rlm-budget-create :calls 4 :tokens 1000 :depth 1)))
    (unwind-protect
         (with-domain-agent-environment
             (:provider provider :configuration configuration :budget budget)
           (test-assert
            (equal (domain-agent-test-triage agent order)
                   '(:object ("eligible" nil)))
            "CodeAct accepts fenced forms and JSON false as NIL"))
      (uiop:delete-directory-tree
       (test-configuration-root configuration)
       :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-domain-agent-source-membership () null)
(defun test-domain-agent-source-membership ()
  "Test extension Lisp is part of the active-image source set."
  (let* ((source-root (asdf:system-source-directory :autolith))
         (paths (active-image-source-paths source-root)))
    (test-assert
     (find-if (lambda (path)
                (search "nooa-autolith-extension/protocol.lisp" path))
              paths)
     "active-image provenance includes domain-agent protocol source")
    (test-assert
     (find "preview.lisp" (project-lisp-pathnames source-root)
           :key #'file-namestring
           :test #'string=)
     "project Lisp discovery includes the domain-agent extension"))
  nil)

(-> test-domain-agent-extension () null)
(defun test-domain-agent-extension ()
  "Run the domain-agent extension tests."
  (test-value-preview)
  (test-domain-agent-catalog)
  (test-domain-agent-predict)
  (test-domain-agent-code-act)
  (test-domain-agent-source-membership)
  nil)
