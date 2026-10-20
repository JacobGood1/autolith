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
