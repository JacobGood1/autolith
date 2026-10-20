(in-package #:autolith)

;;;; -- Diagnosis Commands --

;;; (diagnoses) lists the saved diagnoses of the workspace and its recent
;;; cases: what remained, the repair it points to, and the facts that ruled
;;; out the other hypotheses.

(-> application-diagnoses-entry (application) list)
(defun application-diagnoses-entry (application)
  "Return the saved diagnoses and recent diagnosis cases of APPLICATION's workspace."
  (let* ((configuration (application-configuration application))
         (base (knowledge-base-read configuration))
         (workspace (knowledge--workspace configuration))
         (saved (knowledge-base-diagnoses base :workspace workspace))
         (cases (knowledge-base-diagnosis-cases base workspace)))
    (if (and (null saved) (null cases))
        (list (terminal-span ':hint (format nil "No diagnoses yet.~:[ Enable Diagnostic probes in (settings) to let the model test hypotheses with probes.~;~]"
                                            (config :diagnosis-p configuration))))
        (append
         (list (terminal-span ':brand "diagnoses")
               (terminal-span ':dim (format nil "  ~D saved · ~D recent case~:P~%"
                                            (length saved) (length cases))))
         (loop for diagnosis in saved
               for properties = (rest diagnosis)
               append (list (terminal-span ':strong (format nil "  ~A" (getf properties :id)))
                            (terminal-span ':dim
                                           (format nil "  ~{~A~^, ~} · probes ~{~A~^, ~}~%"
                                                   (mapcar (lambda (hypothesis)
                                                             (getf hypothesis :id))
                                                           (getf properties :hypotheses))
                                                   (mapcar (lambda (probe) (getf probe :tool))
                                                           (getf properties :probes))))))
         (loop for case in cases
               append (application-diagnoses--case-lines case))))))

(-> application-diagnoses--case-lines (list) list)
(defun application-diagnoses--case-lines (case)
  "Return the terminal lines describing one recorded diagnosis CASE."
  (let ((remaining (getf case :remaining))
        (label (format nil "~A~{ ~A=~A~}" (getf case :diagnosis)
                       (loop for (variable value) in (getf case :bindings)
                             append (list (string-left-trim "?" variable)
                                          (sanitize-text (princ-to-string value)
                                                         :single-line-p t))))))
    (append
     (cond
       ((null remaining)
        (list (terminal-span ':failure "  × ")
              (terminal-span ':plain (format nil "~A: every hypothesis was ruled out~%" label))))
       ((rest remaining)
        (list (terminal-span ':notice "  ? ")
              (terminal-span ':plain (format nil "~A: ~{~A~^, ~} remain open~%" label remaining))))
       (t
        (list (terminal-span ':success "  ✓ ")
              (terminal-span ':plain (format nil "~A → ~A~%" label (first remaining)))
              (terminal-span ':dim (if (getf case :repair)
                                       (format nil "      repair: ~A~%"
                                               (sanitize-text (getf case :repair)
                                                              :single-line-p t))
                                       "")))))
     (loop for entry in (getf case :eliminated)
           collect (terminal-span ':dim
                                  (format nil "      ruled out ~A: ~A~%" (getf entry :hypothesis)
                                          (sanitize-text (getf entry :text) :single-line-p t)))))))


;;;; -- Command Definitions --

(define-application-command application--builtin-diagnoses-command
    (:name "/diagnoses"
     :argument nil
     :description "list diagnoses and what decided their cases"
     :tip "shows saved diagnoses and recent cases: the cause found, its repair, and the facts that ruled out the rest."
     :busy-behavior :inspect
     :terminal-behavior :shared
     :callable t)
    (application)
  (application-present application (application-diagnoses-entry application))
  ':continue)
