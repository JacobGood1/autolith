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

(-> learning-tests--archive-turn (conversation integer &key (:recipient string)) null)
(defun learning-tests--archive-turn (conversation year &key (recipient "team"))
  "Record a turn archiving YEAR's invoices and telling RECIPIENT, in two tool calls."
  (conversation-append-user-message conversation (format nil "Archive invoices of ~D" year))
  (learning-tests--call conversation
                        :call-id   (format nil "a~D-1" year)
                        :tool      "invoices.archive"
                        :arguments (json-object "year" year "compress" t)
                        :output    (json-object "archive" (json-object "path"
                                                                       (format nil "/a/~D.tar" year))))
  (learning-tests--call conversation
                        :call-id   (format nil "a~D-2" year)
                        :tool      "notify.send"
                        :arguments (json-object "to" recipient
                                                "message" (format nil "/a/~D.tar" year))
                        :output    (json-object "sent" t))
  (learning-tests--request conversation (format nil "a~D-r" year) 900)
  nil)

(-> test-procedure-mining () null)
(defun test-procedure-mining ()
  "Test generalizing recurring tool sequences into procedures that explain each source turn."
  (with-test-configuration (configuration)
    (let ((reports (conversation-create configuration))
          (archives (conversation-create configuration)))
      (dolist (quarter '("q1" "q3" "q4"))
        (learning-tests--report-turn reports quarter))
      (dolist (year '(2021 2022 2023))
        (learning-tests--archive-turn archives year))
      (let* ((traces (action-traces-in-conversation (conversation-pathname reports)))
             (procedure (procedure-mine traces)))
        (test-assert (equal procedure
                            '(:procedure
                              :id "make-sales-report"
                              :parameters ((:name "file" :type :string))
                              :input ("Make" "the" "sales" "report" "for" (:param "file" "" "."))
                              :steps ((:tool "sales.load"
                                       :arguments (:object ("file" (:text "data/" "file" ".csv"))))
                                      (:tool "sales.report"
                                       :arguments (:object ("table" (:from 0 ("table")))
                                                           ("title" (:text "Sales " "file" ""))))
                                      (:tool "notify.send"
                                       :arguments (:object ("to" (:json "\"team\""))
                                                           ("message"
                                                            (:json "\"The sales report is ready.\"")))))))
                     "varying words, earlier results, and constants become one procedure")
        (test-assert (every (lambda (trace) (procedure-explains-trace-p procedure trace)) traces)
                     "the procedure reproduces every turn it was learned from"))
      (let ((procedure (procedure-mine (action-traces-in-conversation
                                        (conversation-pathname archives)))))
        (test-assert (and (equal (getf (rest procedure) :parameters)
                                 '((:name "year" :type :integer)))
                          (equal (getf (rest procedure) :input)
                                 '("Archive" "invoices" "of" (:param "year" "" "")))
                          (equal (procedure-match-input procedure "archive invoices of 2024")
                                 '(("year" . 2024))))
                     "a number in the request becomes an integer parameter")
        (test-assert (equal (getf (second (getf (rest procedure) :steps)) :arguments)
                            '(:object ("to" (:json "\"team\""))
                              ("message" (:from 0 ("archive" "path")))))
                     "a nested result field feeds a later call"))
      (loop for (description turns expected) in
            '(("two turns are too few" ((2021 "team") (2022 "team")) nil)
              ("three agreeing turns are learned" ((2021 "team") (2022 "team") (2023 "team")) t)
              ("a value seen once is not generalized"
               ((2021 "team") (2022 "team") (2023 "board")) nil)
              ("a value varying throughout becomes a parameter"
               ((2021 "ann") (2022 "bob") (2023 "cy")) t))
            do (let ((conversation (conversation-create configuration)))
                 (loop for (year recipient) in turns
                       do (learning-tests--archive-turn conversation year :recipient recipient))
                 (test-assert (eq expected
                                  (and (procedure-learn-candidates
                                        (reverse (action-traces-in-conversation
                                                  (conversation-pathname conversation))))
                                       t))
                              description)))))
  nil)

(-> test-procedure-learning () null)
(defun test-procedure-learning ()
  "Test recording learned procedures once, keeping their names, and never relearning retired ones."
  (with-test-configuration (configuration)
    (let ((conversation (conversation-create configuration))
          (workspace (knowledge--workspace configuration)))
      (learning-tests--report-turn conversation "q1")
      (learning-tests--report-turn conversation "q2")
      (test-assert (null (procedure-learn configuration)) "two turns teach nothing yet")
      (learning-tests--report-turn conversation "q3" :requests 5)
      (test-assert (equal (procedure-learn configuration) '(("make-sales-report" 1 3)))
                   "the third turn teaches a procedure")
      (let ((version (procedure-entry-version
                      (knowledge-base-procedure (knowledge-base-read configuration)
                                                workspace "make-sales-report"))))
        (test-assert (and (equal (getf version :baseline) '(:requests 4 :tokens 6500))
                          (= 3 (length (getf version :examples))))
                     "a procedure records its source turns and their average cost"))
      (learning-tests--report-turn conversation "q4")
      (test-assert (null (procedure-learn configuration))
                   "another agreeing turn does not add a version")
      (test-assert (null (procedure-learn (configuration-copy configuration :knowledge-mode "off")))
                   "nothing is learned when knowledge is off")
      (procedure-retire configuration "make-sales-report")
      (learning-tests--report-turn conversation "q5")
      (test-assert (null (procedure-learn configuration))
                   "a retired procedure is not learned again")))
  nil)
