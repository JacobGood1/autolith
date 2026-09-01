(in-package #:autolith)

;;;; -- Predict Strategy --

(-> domain-agent--predict-task
    (domain-agent domain-capability list)
    string)
(defun domain-agent--predict-task (agent capability arguments)
  "Return the governing Predict task for CAPABILITY on AGENT."
  (format nil
          "Implement the domain-agent method ~A on ~A.
The method documentation is the governing instruction.
Reply in exactly the requested contract shape. Do not call tools.
Do not ask questions. Do not wrap the answer in commentary.

Method: ~A
Arguments: ~{~A~^, ~}
Documentation:
~A

Agent catalog:
~A"
          (domain-capability-name capability)
          (class-name (class-of agent))
          (domain-capability-name capability)
          (domain-capability-parameter-names capability)
          (let ((documentation (domain-capability-documentation capability)))
            (if (non-empty-string-p documentation)
                documentation
                "No additional documentation."))
          (domain-agent-document agent)))

(-> domain-agent--predict
    (domain-agent domain-capability list
     &key (:provider model-provider)
          (:configuration configuration)
          (:budget rlm-budget))
    t)
(defun domain-agent--predict
    (agent capability arguments &key provider configuration budget)
  "Run CAPABILITY as one contracted inference over live argument previews."
  (multiple-value-bind (value trace-identifier)
      (infer (domain-agent--predict-task agent capability arguments)
             :context (domain-agent--argument-views
                       (domain-capability-parameter-names capability)
                       arguments)
             :contract (domain-capability-contract capability)
             :budget budget
             :provider provider
             :configuration configuration)
    (declare (ignore trace-identifier))
    value))
