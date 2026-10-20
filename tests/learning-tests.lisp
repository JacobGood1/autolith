(in-package #:autolith)

;;;; -- Procedure Learning Tests --

(-> learning-tests--call
    (conversation &key (:call-id string) (:tool string) (:arguments json-object)
                  (:output json-object) (:success-p boolean))
    null)
(defun learning-tests--call (conversation &key call-id tool arguments output (success-p t))
  "Record that the model called TOOL, a NAMESPACE.NAME string, with ARGUMENTS and received OUTPUT."
  (let ((dot (position #\. tool)))
    (conversation-append-provider-item
     conversation
     (json-object "type" "function_call" "call_id" call-id
                  "namespace" (subseq tool 0 dot) "name" (subseq tool (1+ dot))
                  "arguments" (json-encode arguments))))
  (conversation-append-tool-result conversation call-id
                                   :tool-name tool
                                   :output    (json-encode output)
                                   :success-p success-p)
  nil)

(-> learning-tests--request (conversation string integer) null)
(defun learning-tests--request (conversation response tokens)
  "Record one model request RESPONSE that used TOKENS."
  (conversation-append-provider-metadata
   conversation
   (list :request-number 1 :response-id response
         :usage (list (list "input_tokens" tokens)
                      (list "output_tokens" 0)
                      (list "total_tokens" tokens))))
  nil)

(-> learning-tests--report-turn (conversation string &key (:requests integer) (:success-p boolean))
    null)
(defun learning-tests--report-turn (conversation quarter &key (requests 4) (success-p t))
  "Record a turn producing QUARTER's sales report in three tool calls over REQUESTS requests."
  (let ((file (format nil "data/~A.csv" quarter))
        (table (format nil "t-~A" quarter)))
    (conversation-append-user-message conversation
                                      (format nil "Make the sales report for ~A." quarter))
    (learning-tests--call conversation
                          :call-id   (format nil "~A-1" quarter)
                          :tool      "sales.load"
                          :arguments (json-object "file" file)
                          :output    (json-object "rows" 12 "table" table))
    (learning-tests--call conversation
                          :call-id   (format nil "~A-2" quarter)
                          :tool      "sales.report"
                          :arguments (json-object "table" table
                                                  "title" (format nil "Sales ~A" quarter))
                          :output    (json-object "report" (format nil "reports/~A.md" quarter))
                          :success-p success-p)
    (learning-tests--call conversation
                          :call-id   (format nil "~A-3" quarter)
                          :tool      "notify.send"
                          :arguments (json-object "to" "team"
                                                  "message" "The sales report is ready.")
                          :output    (json-object "sent" t))
    (dotimes (index requests)
      (learning-tests--request conversation (format nil "~A-r~D" quarter index) 1500)))
  nil)

(-> test-action-traces () null)
(defun test-action-traces ()
  "Test reading tool-call traces and their model cost from conversation logs."
  (with-test-configuration (configuration)
    (let ((conversation (conversation-create configuration)))
      (learning-tests--report-turn conversation "q1")
      (learning-tests--report-turn conversation "q2" :success-p nil)
      (learning-tests--report-turn conversation "q3" :requests 5)
      (let ((traces (action-traces-in-conversation (conversation-pathname conversation))))
        (test-assert (= 3 (length traces)) "each user turn is one trace")
        (test-assert (equal (action-trace-skeleton (first traces))
                            '("sales.load" "sales.report" "notify.send"))
                     "a trace lists its tool calls in order")
        (let ((call (second (action-trace-calls (third traces)))))
          (test-assert (and (equal (json-get (getf call :arguments) "table") "t-q3")
                            (eq (getf call :status) ':ok)
                            (search "reports/q3.md" (getf call :output)))
                       "each call keeps its arguments, status, and output"))
        (test-assert (and (= 5 (action-trace-requests (third traces)))
                          (= 7500 (action-trace-tokens (third traces))))
                     "a trace counts its model requests and tokens")
        (test-assert (equal (mapcar #'action-trace-accepted-p traces) '(t nil t))
                     "a turn with a failed call is not accepted for learning")
        (learning-tests--report-turn conversation "q4" :requests 2)
        (let ((extended (action-traces-in-conversation (conversation-pathname conversation))))
          (test-assert (and (= 4 (length extended))
                            (eq (first extended) (first traces))
                            (= 2 (action-trace-requests (fourth extended)))
                            (= 5 (action-trace-requests (third traces))))
                       "a later read continues from the cached traces without changing them"))
        (test-assert (null (action-traces-in-conversation (conversation-pathname conversation)
                                                          :workspace "/elsewhere/"))
                     "a conversation from another workspace yields no traces"))
      (test-assert (equal (mapcar (lambda (trace) (action-trace-input trace))
                                  (action-traces configuration))
                          '("Make the sales report for q4." "Make the sales report for q3."
                            "Make the sales report for q1."))
                   "recent accepted traces of the workspace come newest first")))
  nil)

(-> learning-tests--procedure () list)
(defun learning-tests--procedure ()
  "Return a two-step report procedure taking a quarter."
  '(:procedure :id "make-sales-report"
    :parameters ((:name "quarter" :type :string))
    :input ("Make" "the" "sales" "report" "for" (:param "quarter" "" "."))
    :steps ((:tool "sales.load"
             :arguments (:object ("file" (:text "data/" "quarter" ".csv"))))
            (:tool "sales.report"
             :arguments (:object ("table" (:from 0 ("table")))
                                 ("title" (:text "Sales " "quarter" ""))
                                 ("format" (:json "\"markdown\"")))))))

(-> test-procedure-templates () null)
(defun test-procedure-templates ()
  "Test filling and matching procedure argument templates and request patterns."
  (let* ((procedure (learning-tests--procedure))
         (steps (getf (rest procedure) :steps))
         (bindings (list (cons "quarter" "q5")))
         (outputs (list (json-decode "{\"rows\":12,\"table\":\"t-q5\"}")))
         (report (procedure-template-instantiate (getf (second steps) :arguments)
                                                 bindings outputs)))
    (test-assert (procedure-p procedure) "a well-formed procedure validates")
    (test-assert (and (equal (json-get report "table") "t-q5")
                      (equal (json-get report "title") "Sales q5")
                      (equal (json-get report "format") "markdown"))
                 "instantiation fills parameters, earlier results, and constants")
    (test-assert (eq ':missing-output
                     (handler-case (procedure-template-instantiate
                                    (getf (second steps) :arguments)
                                    bindings (list (json-object "rows" 3)))
                       (knowledge-error (condition) (knowledge-error-code condition))))
                 "a result without the expected field stops instantiation")
    (multiple-value-bind (found matched-p)
        (procedure-template-match (getf (second steps) :arguments) report nil outputs)
      (test-assert (and matched-p (equal (rest (assoc "quarter" found :test #'string=)) "q5"))
                   "matching recovers the parameters an instantiation used"))
    (test-assert (not (nth-value 1 (procedure-template-match
                                    (getf (second steps) :arguments)
                                    (json-decode "{\"table\":\"t-q5\",\"title\":\"Sales q6\",\"format\":\"markdown\"}")
                                    (list (cons "quarter" "q5")) outputs)))
                 "matching refuses a parameter bound to two values")
    (loop for (text expected) in '(("make the sales report for q7." "q7")
                                   ("Make the sales report for q7" nil)
                                   ("Make the sales report for ." nil)
                                   ("Make the weekly report for q7." nil))
          do (multiple-value-bind (found matched-p) (procedure-match-input procedure text)
               (test-assert (equal (and matched-p (rest (assoc "quarter" found :test #'string=)))
                                   expected)
                            (format nil "request ~S binds ~S" text expected))))
    (dolist (broken (list (list* :procedure :id "x" :parameters nil
                                 :steps '((:tool "a.b" :arguments (:object ("x" (:param "q")))))
                                 nil)
                          (list* :procedure :id "x" :parameters nil
                                 :steps '((:tool "a.b" :arguments (:object ("x" (:from 0 ("y"))))))
                                 nil)))
      (test-assert (not (procedure-p broken))
                   "a procedure naming an unknown parameter or a later step is refused")))
  nil)

(-> test-procedure-lifecycle () null)
(defun test-procedure-lifecycle ()
  "Test recording, admitting, suspending, and retiring a learned procedure."
  (with-test-configuration (configuration)
    (let ((procedure (learning-tests--procedure))
          (workspace (knowledge--workspace configuration)))
      (flet ((entry ()
               (knowledge-base-procedure (knowledge-base-read configuration)
                                         workspace "make-sales-report"))
             (run (status)
               (procedure-record-run configuration "make-sales-report"
                                     :version 1 :status status :steps 1 :caller ':model
                                     :failure (and (eq status ':failed) "sales.report failed"))))
        (test-assert (nth-value 2 (procedure-define configuration procedure
                                                    :examples '((:conversation "c" :sequence 1))
                                                    :baseline '(:requests 4 :tokens 6000)))
                     "a new procedure is recorded")
        (test-assert (and (not (nth-value 2 (procedure-define configuration procedure)))
                          (= 1 (length (getf (entry) :versions))))
                     "recording an equal procedure again adds no version")
        (test-assert (and (eq (procedure-entry-status (entry)) ':candidate)
                          (null (procedure-entry-runnable-version (entry))))
                     "a learned procedure waits for admission")
        (procedure-admit configuration "make-sales-report")
        (test-assert (eq (procedure-entry-status (entry)) ':admitted)
                     "an admitted procedure may run")
        (run ':failed)
        (run ':ok)
        (run ':failed)
        (test-assert (eq (procedure-entry-status (entry)) ':admitted)
                     "a success resets the failure streak")
        (run ':failed)
        (test-assert (and (eq (procedure-entry-status (entry)) ':suspended)
                          (= 4 (getf (entry) :runs))
                          (= 3 (getf (entry) :failures))
                          (equal (getf (entry) :last-failure) "sales.report failed"))
                     "consecutive failures suspend the procedure and the ledger counts runs")
        (procedure-admit configuration "make-sales-report")
        (test-assert (eq (procedure-entry-status (entry)) ':admitted)
                     "admitting again lifts a suspension")
        (procedure-retire configuration "make-sales-report")
        (test-assert (eq (procedure-entry-status (entry)) ':retired)
                     "a retired procedure no longer runs")
        (test-assert (eq ':unknown-procedure
                         (handler-case (procedure-admit configuration "missing")
                           (knowledge-error (condition) (knowledge-error-code condition))))
                     "admitting an unknown procedure is refused"))))
  nil)
