(in-package #:autolith)

;;;; -- Contracted Tool Calls --

;;; Every tool call passes through its contracts, when any describe it. In
;;; active mode a call whose requirements do not hold is refused before it
;;; runs. In active and passive mode a successful call records its predicted
;;; effects and its observations. Calls no contract describes run unchanged.

(-> tool-execute-with-contracts (tool tool-context json-object) tool-result)
(defun tool-execute-with-contracts (tool context arguments)
  "Execute TOOL with ARGUMENTS under CONTEXT, honoring the contracts describing the call."
  (let* ((configuration (tool-context-configuration context))
         (name (tool-canonical-name tool))
         (matches (and configuration
                       (knowledge-capture-p configuration)
                       (handler-case (knowledge-contracts-for-call configuration name arguments)
                         (knowledge-error (condition)
                           (return-from tool-execute-with-contracts
                             (tool-failure
                              (format nil "A contract for ~A could not read the call: ~A The call was not made."
                                      name (autolith-error-message condition))
                              :code ':precondition-unknown)))))))
    (if (null matches)
        (tool-execute tool context arguments)
        (let ((refusal (and (knowledge-active-p configuration)
                            (knowledge-contract-refusal configuration matches))))
          (if refusal
              (tool-failure refusal :code ':precondition-unmet)
              (let ((result (tool-execute tool context arguments)))
                (when (tool-result-success-p result)
                  (knowledge-contract-record
                   configuration matches arguments result
                   :tool name
                   :conversation (and (tool-context-conversation context)
                                      (conversation-identifier
                                       (tool-context-conversation context)))))
                result))))))
