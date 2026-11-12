(in-package #:autolith)

;;;; -- Antigravity CLI Provider Tests --

(defclass antigravity-test-eval-tool (tool)
  ((calls :initform 0 :accessor antigravity-test-eval-tool-calls
          :documentation "Count of mock tool executions."))
  (:documentation "Deterministic test tool used to exercise Antigravity CLI turns."))

(defmethod tool-execute ((tool antigravity-test-eval-tool) context arguments)
  "Multiply 6 and 7 for the mock Antigravity test turn."
  (declare (ignore context arguments))
  (incf (antigravity-test-eval-tool-calls tool))
  (tool-success "42"))

(defclass antigravity-test-observer (agent-observer)
  ((text      :initform "" :accessor antigravity-test-observer-text
              :documentation "Accumulated assistant text deltas.")
   (reasoning :initform "" :accessor antigravity-test-observer-reasoning
              :documentation "Accumulated reasoning deltas."))
  (:documentation "Observer collecting text and reasoning deltas during Antigravity tests."))

(defmethod agent-observer-text ((observer antigravity-test-observer) text)
  "Append streamed assistant TEXT."
  (setf (antigravity-test-observer-text observer)
        (concatenate 'string (antigravity-test-observer-text observer) text)))

(defmethod agent-observer-reasoning ((observer antigravity-test-observer) text)
  "Append streamed reasoning TEXT."
  (setf (antigravity-test-observer-reasoning observer)
        (concatenate 'string (antigravity-test-observer-reasoning observer) text)))

#-win32
(-> test-antigravity--write-mock-cli (pathname pathname) string)
#-win32
(defun test-antigravity--write-mock-cli (directory calls-file)
  "Write an executable mock `agy` shell script inside DIRECTORY and return its namestring."
  (let ((script    (merge-pathnames "mock-agy.sh" directory))
        (step1-out (json-encode
                    (json-object
                     "event" "result"
                     "result"
                     (json-object
                      "status" "SUCCESS"
                      "conversation_id" "conv-1"
                      "usage" (json-object "input_tokens" 25 "output_tokens" 12 "total_tokens" 37)
                      "structured_output"
                      (json-object
                       "text" ""
                       "tool_calls" (json-array
                                     (json-object "name" "reaper.eval"
                                                  "arguments" "{\"code\":\"(* 6 7)\"}")))))))
        (step2-spill (json-encode
                      (json-object
                       "event" "result"
                       "result"
                       (json-object
                        "status" "SUCCESS"
                        "conversation_id" "conv-2"
                        "usage" (json-object "input_tokens" 40 "output_tokens" 10
                                             "thinking_tokens" 5 "total_tokens" 55)
                        "structured_output"
                        (json-object "text" "spill=yes product=42"
                                     "tool_calls" (json-array))))))
        (step2-inline (json-encode
                       (json-object
                        "event" "result"
                        "result"
                        (json-object
                         "status" "SUCCESS"
                         "conversation_id" "conv-2"
                         "usage" (json-object "input_tokens" 40 "output_tokens" 10
                                              "thinking_tokens" 5 "total_tokens" 55)
                         "structured_output"
                         (json-object "text" "spill=no product=42"
                                      "tool_calls" (json-array))))))
        (think-1 (json-encode
                  (json-object "event" "step_update"
                               "step_update"
                               (json-object "step_type" "agent_response"
                                            "thinking_delta" "planning call"))))
        (think-2 (json-encode
                  (json-object "event" "step_update"
                               "step_update"
                               (json-object "step_type" "agent_response"
                                            "thinking_delta" "verifying result")))))
    (with-open-file (out script
                         :direction         :output
                         :if-exists         :supersede
                         :if-does-not-exist :create
                         :external-format   :utf-8)
      (write-line "#!/bin/sh" out)
      (format out "printf '%s\\n' \"$*\" >> '~A'~%" (namestring calls-file))
      (write-line "if [ \"$1\" = \"models\" ]; then" out)
      (write-line "  printf '  gemini-4-carbon-high  Gemini 4 Carbon (High)\\n  gemini-3.8-flash-low  Gemini 3.8 Flash (Low)\\n'" out)
      (write-line "  exit 0" out)
      (write-line "fi" out)
      (write-line "if grep -q '\\[TOOL_OUTPUT' prompt.txt 2>/dev/null || printf '%s' \"$*\" | grep -q '\\[TOOL_OUTPUT'; then" out)
      (format out "  cat <<'EOF'~%~A~%EOF~%" think-2)
      (write-line "  if [ -f prompt.txt ]; then" out)
      (format out "    cat <<'EOF'~%~A~%EOF~%" step2-spill)
      (write-line "  else" out)
      (format out "    cat <<'EOF'~%~A~%EOF~%" step2-inline)
      (write-line "  fi" out)
      (write-line "else" out)
      (format out "  cat <<'EOF'~%~A~%~A~%EOF~%" think-1 step1-out)
      (write-line "fi" out))
    (sb-posix:chmod (namestring script) #o755)
    (namestring script)))

(-> test-antigravity-provider-registration-and-parsing () null)
(defun test-antigravity-provider-registration-and-parsing ()
  "Verify built-in `antigravity` / `agy` registration, effort suppression, and `agy models` parsing."
  (let ((registration (provider-registration-find "antigravity"))
        (alias        (provider-registration-find "agy")))
    (test-assert (and registration (eq registration alias))
                 "`antigravity` and its `agy` alias resolve to the same built-in registration.")
    (dolist (model '("gemini-4-carbon" "gemini-4-carbon-high" "gemini-3.8-flash-low"))
      (multiple-value-bind (resolved found)
          (provider-resolve-model model :provider "agy")
        (test-assert (and (string= resolved model) (eq found registration))
                     "Antigravity models resolve cleanly under the `agy` alias.")))
    (test-assert (equal (antigravity--wire-effort "gemini-4-carbon" "high") "high")
                 "Base Antigravity models forward `--effort`.")
    (test-assert (null (antigravity--wire-effort "gemini-3.8-flash-low" "high"))
                 "Effort-suffixed Antigravity models omit conflicting `--effort`.")
    (let ((parsed (antigravity--parse-models-output
                   (format nil "~C| Loading models...~C  ~%Available models:~%  gemini-4-carbon-high  Gemini 4 Carbon (High)~%- gemini-3.8-flash-low~%  future-model-max  Future Max~%"
                           #\Return #\Return))))
      (test-assert (equal (mapcar (lambda (entry) (getf entry :name)) parsed)
                          '("gemini-4-carbon-high" "gemini-3.8-flash-low" "future-model-max"))
                   "`agy models` parser strips carriage-return spinners and preserves model IDs."))
    (let ((wrapped (antigravity--unwrap-structured-payload
                    (json-object "text" "{\"text\":\"391\",\"tool_calls\":[]}"
                                 "tool_calls" (json-array)))))
      (test-assert (equal (json-get wrapped "text") "391")
                   "Double-wrapped structured envelopes unwrap cleanly to plain text.")))
  nil)

#-win32
(-> test-antigravity-provider-mock-turn-and-spillover () null)
#-win32
(defun test-antigravity-provider-mock-turn-and-spillover ()
  "Exercise streaming thinking deltas, structured tool calls, and >64 KB `prompt.txt` spillover."
  (with-test-configuration (base)
    (let* ((root       (test-configuration-root base))
           (calls-file (merge-pathnames "agy-invocations.log" root))
           (mock-cli   (test-antigravity--write-mock-cli root calls-file)))
      (with-test-environment (("AUTOLITH_ANTIGRAVITY_COMMAND" mock-cli))
        (let* ((configuration (configuration-copy base
                                                  :model          "gemini-3.8-flash-low"
                                                  :model-provider "antigravity"))
               (provider      (provider-create configuration :reasoning-summaries-p t))
               (conversation  (conversation-create configuration))
               (registry      (make-instance 'tool-registry))
               (eval-tool     (make-instance 'antigravity-test-eval-tool
                                             :namespace   "reaper"
                                             :name        "eval"
                                             :description "Evaluate code."
                                             :parameters  (tool-object-schema
                                                           (json-object "code" (json-object "type" "string"))
                                                           '("code"))))
               (observer      (make-instance 'antigravity-test-observer)))
          (tool-registry-describe-namespace registry "reaper" "Reaper operations.")
          (tool-registry-register registry eval-tool)
          (let ((*antigravity-inline-prompt-limit* 64)
                (agent (agent-create :configuration configuration
                                     :provider      provider
                                     :conversation  conversation
                                     :tool-registry registry)))
            (agent-run-user-turn agent "Compute 6 * 7" :observer observer)
            (test-assert (= (antigravity-test-eval-tool-calls eval-tool) 1)
                         "Mock `agy` structured tool call executes host `reaper.eval` once.")
            (test-assert (search "planning call" (antigravity-test-observer-reasoning observer))
                         "Streaming `thinking_delta` reaches the agent observer.")
            (test-assert (equal (antigravity-test-observer-text observer) "spill=yes product=42")
                         "Oversized prompt spills to `prompt.txt` and streams final assistant text.")
            (let ((invocations (uiop:read-file-string calls-file)))
              (test-assert (not (search "--effort" invocations))
                           "`gemini-3.8-flash-low` omits `--effort` on the CLI command line.")))))))
  nil)
