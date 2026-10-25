(in-package #:autolith)

;;;; -- Mechanical Precompaction Tests --

(-> precompaction-tests--call (string) json-object)
(defun precompaction-tests--call (call-id)
  "Return a completed shell call named CALL-ID."
  (json-object "type" "function_call" "status" "completed" "arguments" "{}"
               "call_id" call-id "name" "run" "namespace" "shell"))

(-> precompaction-tests--exchange (conversation string &key (:success-p boolean)) null)
(defun precompaction-tests--exchange (conversation call-id &key success-p)
  "Append a call named CALL-ID and its result, failed unless SUCCESS-P, to CONVERSATION."
  (conversation-append-provider-item conversation (precompaction-tests--call call-id))
  (conversation-append-tool-result conversation call-id
                                   :tool-name "shell.run"
                                   :output    (if success-p "done" "command failed")
                                   :success-p success-p)
  nil)

(-> precompaction-tests--call-ids (conversation) list)
(defun precompaction-tests--call-ids (conversation)
  "Return the call identifiers of CONVERSATION's projected calls, in order."
  (loop for item in (coerce (conversation-input-items conversation) 'list)
        when (clinker-transcript:function-call-item-p item)
          collect (json-get item "call_id")))

(-> test-precompaction-drops-failed-exchanges () null)
(defun test-precompaction-drops-failed-exchanges ()
  "Drop failed exchanges of earlier turns with their reasoning, and keep everything else."
  (with-test-configuration (configuration)
    (let ((conversation (conversation-create configuration :identifier "precompaction"))
          (reasoning    (json-object "type" "reasoning" "encrypted_content" "opaque"
                                     "summary" (json-array))))
      (conversation-append-user-message conversation "start")
      (conversation-append-provider-item conversation reasoning)
      (precompaction-tests--exchange conversation "failed-early")
      (precompaction-tests--exchange conversation "succeeded" :success-p t)
      (conversation-append-provider-item conversation (precompaction-tests--call "steered"))
      (conversation-append-user-message conversation "next")
      (conversation-append-tool-result conversation "steered"
                                       :tool-name "shell.run" :output "denied"
                                       :success-p nil)
      (precompaction-tests--exchange conversation "failed-current")
      (let ((records (length (conversation--read-records
                              (conversation-pathname conversation)))))
        (test-assert (= 1 (conversation-precompact conversation))
                     "one earlier failed exchange is dropped")
        (test-assert (equal (precompaction-tests--call-ids conversation)
                            '("succeeded" "steered" "failed-current"))
                     "successful calls, split exchanges and the current turn's failures stay")
        (test-assert (not (member reasoning (coerce (conversation-input-items conversation) 'list)))
                     "reasoning that only led to a dropped call goes with it")
        (test-assert (not (find "failed-early" (coerce (conversation-input-items conversation) 'list)
                                :key (lambda (item) (json-get item "call_id")) :test #'equal))
                     "the dropped call's output goes with it")
        (test-assert (= records (length (conversation--read-records
                                         (conversation-pathname conversation))))
                     "the durable log keeps every record")
        (let ((loaded (conversation-load-by-id configuration "precompaction")))
          (test-assert (member "failed-early" (precompaction-tests--call-ids loaded)
                               :test #'equal)
                       "loading alone keeps the durable projection")
          (test-assert (= 1 (conversation-prepare-provider-request
                             loaded :identity (list ':codex "model-a")))
                       "the first request after a resume precompacts from durable records")
          (test-assert (equal (precompaction-tests--call-ids loaded)
                              '("succeeded" "steered" "failed-current"))
                       "the resumed projection matches the live one")))))
  nil)

(-> test-precompaction-cold-requests () null)
(defun test-precompaction-cold-requests ()
  "Precompact only when the provider or model changes or the cache has idled out."
  (with-test-configuration (configuration)
    (let ((conversation (conversation-create configuration :identifier "precompaction-cold"))
          (identity     (list ':codex "model-a")))
      (flet ((prepare (identity now)
               "Prepare a request served by IDENTITY at universal time NOW."
               (conversation-prepare-provider-request conversation
                                                      :identity identity :now now))

             (fail-turn (call-id)
               "Append a failed exchange followed by a new user message."
               (precompaction-tests--exchange conversation call-id)
               (conversation-append-user-message conversation "again")))
        (conversation-append-user-message conversation "start")
        (fail-turn "first")
        (test-assert (zerop (prepare identity 1000))
                     "the first request of a new conversation prunes nothing")
        (test-assert (zerop (prepare identity 1010))
                     "a warm request with the same provider prunes nothing")
        (test-assert (= 1 (prepare (list ':codex "model-b") 1020))
                     "a model switch precompacts")
        (fail-turn "second")
        (test-assert (zerop (prepare (list ':codex "model-b") 1030))
                     "a warm follow-up keeps newer failures")
        (test-assert (= 1 (prepare (list ':codex "model-b")
                                   (+ 1030 *conversation-precompaction-idle-seconds*)))
                     "an idled-out cache precompacts")
        (test-assert (null (precompaction-tests--call-ids conversation))
                     "every earlier failed call is gone"))))
  nil)
