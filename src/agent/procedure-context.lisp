(in-package #:autolith)

;;;; -- Procedure Offers --

;;; When the latest request fits a learned procedure's request pattern, the
;;; model is shown the one procedure.run call that makes the same tool calls
;;; the earlier turns made, with the parameters the request supplies.

(defclass procedure-offer-contribution (context-contribution)
  ((names :initarg :names
          :reader procedure-offer-contribution-names
          :type list
          :documentation "The names of the offered procedures."))
  (:documentation "Advice offering learned procedures that fit the request."))

(defclass procedure-offer-event (provider-event)
  ((names :initarg :names
          :reader procedure-offer-event-names
          :type list
          :documentation "The names of the procedures offered to the model."))
  (:documentation "A presentation event for offered procedures."))

(defmethod context--copy-contribution :around
    ((contribution procedure-offer-contribution) contributor source)
  "Preserve the offered names when attaching contribution provenance."
  (change-class (call-next-method) 'procedure-offer-contribution
                :names (procedure-offer-contribution-names contribution)))

(defmethod context-contribution-provider-event ((contribution procedure-offer-contribution))
  "Project the offered procedure names."
  (make-instance 'procedure-offer-event
                 :names (procedure-offer-contribution-names contribution)))

(-> procedure-offer-line (list) string)
(defun procedure-offer-line (offer)
  "Return the line offering OFFER: its calls and the procedure.run arguments to use."
  (let ((procedure (getf offer :procedure)))
    (format nil "~A (~:[learned, not yet admitted; running it asks for admission~;admitted~]; calls ~{~A~^, ~}): procedure.run ~A"
            (getf offer :id) (eq (getf offer :status) ':admitted)
            (mapcar (lambda (step) (getf step :tool)) (getf (rest procedure) :steps))
            (procedure-describe-call procedure (getf offer :bindings)))))

(-> procedure-offer-context (request-context) (option context-contribution))
(defun procedure-offer-context (request)
  "Offer the learned procedures that fit the latest user input in REQUEST."
  (let ((configuration (request-context-configuration request))
        (text (request-context-latest-user-text request)))
    (when (and (not (request-context-compaction-p request))
               (knowledge-active-p configuration)
               (non-empty-string-p text))
      (let ((offers (handler-case (procedure-offers configuration text)
                      (knowledge-error ()
                        nil))))
        (when offers
          (change-class
           (make-context-contribution
            :identifier "procedure-offers"
            :instruction
            "A procedure Autolith learned from earlier turns fits this request. One procedure.run call with the arguments shown makes the same tool calls those turns made; check its report and answer from it. Work normally instead if the request needs something different."
            :evidence (format nil "~{~A~^~%~}" (mapcar #'procedure-offer-line offers))
            :priority 25
            :lifetime ':turn
            :deduplication-key "procedure-offers")
           'procedure-offer-contribution
           :names (mapcar (lambda (offer) (getf offer :id)) offers)))))))

(register-context-contributor "procedure-offers" 'procedure-offer-context :source ':built-in)

(-> agent-learn-procedures (agent agent-observer integer) null)
(defun agent-learn-procedures (agent observer tool-calls)
  "Learn procedures after a primary AGENT's turn that made TOOL-CALLS calls, reporting new ones.

A turn with fewer than two calls teaches nothing, so it reads no traces. A
failure while learning is not the turn's failure: nothing is reported."
  (let ((child-class (find-class 'task-child-agent nil)))
    (when (and (>= tool-calls 2)
               (not (and child-class (typep agent child-class))))
      (let ((learned (handler-case (procedure-learn (agent-configuration agent))
                       (error ()
                         nil))))
        (when learned
          (agent-observer-status observer :procedures-learned
                                 (list :procedures learned))))))
  nil)
