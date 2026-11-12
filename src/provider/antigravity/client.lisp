(in-package #:autolith)

;;;; -- Antigravity CLI Provider --

(declaim (special *provider-context-admission-state*))

(defparameter *antigravity-inline-prompt-limit* 65536
  "Maximum UTF-8 prompt characters passed inline via --print before spilling to a scratch file.")

(defparameter *antigravity-effort-suffixes*
  '("-low" "-medium" "-high" "-xhigh" "-max")
  "Model identifier suffixes that already pin an Antigravity reasoning effort.")

(defparameter *antigravity-models*
  '((:name              "gemini-4-carbon"
     :description       "Gemini 4 Carbon"
     :context-window    1048576
     :reasoning-efforts ("none" "low" "medium" "high" "xhigh" "max" "ultra"))
    (:name              "gemini-4-argon"
     :description       "Gemini 4 Argon"
     :context-window    1048576
     :reasoning-efforts ("none" "low" "medium" "high" "xhigh" "max" "ultra"))
    (:name              "gemini-3.8-flash"
     :description       "Gemini 3.8 Flash"
     :context-window    1048576
     :reasoning-efforts ("none" "low" "medium" "high" "xhigh" "max" "ultra"))
    (:name              "gemini-4-carbon-high"
     :description       "Gemini 4 Carbon (High)"
     :context-window    1048576
     :reasoning-efforts ("none" "low" "medium" "high" "xhigh" "max" "ultra"))
    (:name              "gemini-4-carbon-medium"
     :description       "Gemini 4 Carbon (Medium)"
     :context-window    1048576
     :reasoning-efforts ("none" "low" "medium" "high" "xhigh" "max" "ultra"))
    (:name              "gemini-4-carbon-low"
     :description       "Gemini 4 Carbon (Low)"
     :context-window    1048576
     :reasoning-efforts ("none" "low" "medium" "high" "xhigh" "max" "ultra"))
    (:name              "gemini-4-argon-high"
     :description       "Gemini 4 Argon (High)"
     :context-window    1048576
     :reasoning-efforts ("none" "low" "medium" "high" "xhigh" "max" "ultra"))
    (:name              "gemini-4-argon-medium"
     :description       "Gemini 4 Argon (Medium)"
     :context-window    1048576
     :reasoning-efforts ("none" "low" "medium" "high" "xhigh" "max" "ultra"))
    (:name              "gemini-4-argon-low"
     :description       "Gemini 4 Argon (Low)"
     :context-window    1048576
     :reasoning-efforts ("none" "low" "medium" "high" "xhigh" "max" "ultra"))
    (:name              "gemini-3.8-flash-high"
     :description       "Gemini 3.8 Flash (High)"
     :context-window    1048576
     :reasoning-efforts ("none" "low" "medium" "high" "xhigh" "max" "ultra"))
    (:name              "gemini-3.8-flash-medium"
     :description       "Gemini 3.8 Flash (Medium)"
     :context-window    1048576
     :reasoning-efforts ("none" "low" "medium" "high" "xhigh" "max" "ultra"))
    (:name              "gemini-3.8-flash-low"
     :description       "Gemini 3.8 Flash (Low)"
     :context-window    1048576
     :reasoning-efforts ("none" "low" "medium" "high" "xhigh" "max" "ultra"))
    (:name              "claude-opus-5-5-high"
     :description       "Claude Opus 5.5 (High)"
     :context-window    200000
     :reasoning-efforts ("none" "low" "medium" "high" "xhigh" "max" "ultra"))
    (:name              "claude-opus-5-5-medium"
     :description       "Claude Opus 5.5 (Medium)"
     :context-window    200000
     :reasoning-efforts ("none" "low" "medium" "high" "xhigh" "max" "ultra"))
    (:name              "claude-opus-5-5-low"
     :description       "Claude Opus 5.5 (Low)"
     :context-window    200000
     :reasoning-efforts ("none" "low" "medium" "high" "xhigh" "max" "ultra"))
    (:name              "claude-sonnet-5-5-high"
     :description       "Claude Sonnet 5.5 (High)"
     :context-window    200000
     :reasoning-efforts ("none" "low" "medium" "high" "xhigh" "max" "ultra"))
    (:name              "claude-sonnet-5-5-medium"
     :description       "Claude Sonnet 5.5 (Medium)"
     :context-window    200000
     :reasoning-efforts ("none" "low" "medium" "high" "xhigh" "max" "ultra"))
    (:name              "claude-sonnet-5-5-low"
     :description       "Claude Sonnet 5.5 (Low)"
     :context-window    200000
     :reasoning-efforts ("none" "low" "medium" "high" "xhigh" "max" "ultra"))
    (:name              "gpt-oss-120b-medium"
     :description       "GPT-OSS 120B (Medium)"
     :context-window    131072
     :reasoning-efforts ("none" "low" "medium" "high" "xhigh" "max" "ultra")))
  "Built-in model catalog served by the Antigravity CLI.")


;;;; -- Executable Discovery and Credentials --

(-> antigravity--regular-file-p ((or pathname string)) (option pathname))
(defun antigravity--regular-file-p (candidate)
  "Return CANDIDATE's resolved pathname when it names an existing file, or NIL."
  (handler-case
      (let ((probed (probe-file candidate)))
        (when (and probed (not (uiop:directory-pathname-p probed)))
          probed))
    (file-error ()
      nil)))

(-> antigravity--search-path (string) (option string))
(defun antigravity--search-path (program)
  "Search PATH for PROGRAM and return its namestring when found."
  (let* ((path-env  (or (uiop:getenv "PATH") ""))
         (separator #+win32 #\; #-win32 #\:)
         (entries   (uiop:split-string path-env :separator (list separator)))
         (names     #+win32 (list (concatenate 'string program ".exe")
                                  (concatenate 'string program ".cmd")
                                  program)
                    #-win32 (list program)))
    (loop for entry in entries
          when (non-empty-string-p entry)
            do (let ((directory (uiop:ensure-directory-pathname entry)))
                 (dolist (name names)
                   (let ((found (antigravity--regular-file-p
                                 (merge-pathnames name directory))))
                     (when found
                       (return-from antigravity--search-path
                         (namestring found)))))))
    nil))

(-> antigravity-command () (option string))
(defun antigravity-command ()
  "Locate the Antigravity CLI binary from AUTOLITH_ANTIGRAVITY_COMMAND, PATH, or ~/.local/bin."
  (let ((override (uiop:getenv "AUTOLITH_ANTIGRAVITY_COMMAND")))
    (cond
      ((non-empty-string-p override)
       (let ((trimmed (string-trim '(#\Space #\Tab) override)))
         (if (or (find #\/ trimmed) (find #\\ trimmed))
             (let ((probed (antigravity--regular-file-p trimmed)))
               (and probed (namestring probed)))
             (antigravity--search-path trimmed))))
      (t
       (or (antigravity--search-path "agy")
           (let ((local-bin
                   (antigravity--regular-file-p
                    (merge-pathnames #+win32 ".local/bin/agy.exe"
                                     #-win32 ".local/bin/agy"
                                     (user-homedir-pathname)))))
             (and local-bin (namestring local-bin))))))))

(-> antigravity-provider-endpoint () string)
(defun antigravity-provider-endpoint ()
  "Return the logical endpoint identity for Antigravity model discovery caching."
  (or (let ((override (uiop:getenv "AUTOLITH_ANTIGRAVITY_COMMAND")))
        (and (non-empty-string-p override)
             (concatenate 'string "agy://" (string-trim '(#\Space #\Tab) override))))
      *antigravity-provider-endpoint*))

(defclass antigravity-credential-manager (credential-manager)
  ()
  (:documentation "Delegates session authentication to the public Antigravity CLI."))

(defmethod credential-manager-provider-label ((manager antigravity-credential-manager))
  "Name the Antigravity CLI service in user-visible diagnostics."
  (declare (ignore manager))
  "Antigravity")

(defmethod credential-manager-login-hint ((manager antigravity-credential-manager))
  "Point credential failures to the documented Antigravity CLI login command."
  (declare (ignore manager))
  "run agy acp --login or autolith auth antigravity")

(defmethod credential-manager-refreshable-p ((manager antigravity-credential-manager))
  "The Antigravity CLI manages token refresh internally during each invocation."
  (declare (ignore manager))
  nil)

(defmethod credential-manager-load ((manager antigravity-credential-manager))
  "Verify that the Antigravity CLI binary is installed without reading private files."
  (declare (ignore manager))
  (let ((command (antigravity-command)))
    (unless command
      (error 'credentials-unavailable
             :message
             "The Antigravity CLI (agy) was not found; install agy or set AUTOLITH_ANTIGRAVITY_COMMAND."))
    (list :command command)))

(defmethod call-with-credentials
    ((manager antigravity-credential-manager) function &key force-refresh)
  "Resolve the Antigravity CLI command within Autolith's secret-use scope."
  (declare (ignore force-refresh))
  (call-with-secret-use
   (lambda ()
     (funcall function (credential-manager-load manager)))))


;;;; -- Provider Class and Model Discovery --

(defclass antigravity-provider
    (session-preserving-provider-mixin subscription-provider)
  ((reasoning-summaries-p
    :initarg       :reasoning-summaries-p
    :initform      nil
    :accessor      provider-reasoning-summaries-p
    :type          boolean
    :documentation "Whether visible reasoning deltas from Antigravity are streamed."))
  (:documentation "Model provider executing turns through the public Antigravity CLI."))

(defmethod provider-account-label ((provider antigravity-provider))
  "Return the Antigravity service name for user-visible diagnostics."
  (declare (ignore provider))
  "Antigravity")

(defmethod provider-family ((provider antigravity-provider))
  "Return the Antigravity conversation family."
  (declare (ignore provider))
  ':antigravity)

(defmethod provider-reconfiguration-initargs append
    ((provider antigravity-provider))
  "Preserve Antigravity reasoning-summary preferences across reconfigurations."
  (list :reasoning-summaries-p (provider-reasoning-summaries-p provider)))

(defmethod provider-set-reasoning-summaries
    ((provider antigravity-provider) (enabled-p t))
  "Update whether Antigravity reasoning deltas are surfaced."
  (check-type enabled-p boolean)
  (setf (provider-reasoning-summaries-p provider) enabled-p)
  provider)

(-> antigravity-provider-create
    (configuration &key (:reasoning-summaries-p boolean))
    antigravity-provider)
(defun antigravity-provider-create (configuration &key reasoning-summaries-p)
  "Create an Antigravity CLI provider for CONFIGURATION."
  (make-instance 'antigravity-provider
                 :configuration         configuration
                 :credential-manager    (make-instance 'antigravity-credential-manager)
                 :session-id            (make-identifier)
                 :reasoning-summaries-p reasoning-summaries-p))

(defmethod provider-family-create
    ((family (eql ':antigravity))
     (configuration configuration)
     &key reasoning-summaries-p)
  "Create the built-in Antigravity CLI provider."
  (antigravity-provider-create configuration
                               :reasoning-summaries-p reasoning-summaries-p))

(-> antigravity--default-window-for (string) (integer 1))
(defun antigravity--default-window-for (model)
  "Return the known context window for MODEL."
  (or (getf (find model *antigravity-models*
                  :key (lambda (spec) (getf spec :name))
                  :test #'string=)
            :context-window)
      (cond
        ((uiop:string-prefix-p "gemini-" model)
         1048576)
        ((uiop:string-prefix-p "claude-" model)
         200000)
        (t
         131072))))

(-> antigravity--clean-terminal-line (string) string)
(defun antigravity--clean-terminal-line (line)
  "Drop carriage-return spinner segments from LINE and trim surrounding whitespace."
  (let* ((cr-pos (position #\Return line :from-end t))
         (tail   (if cr-pos (subseq line (1+ cr-pos)) line)))
    (string-trim '(#\Space #\Tab #\Newline #\Return) tail)))

(-> antigravity--model-token-p (string) boolean)
(defun antigravity--model-token-p (token)
  "Return true when TOKEN has the lowercase identifier syntax of an agy model ID."
  (and (plusp (length token))
       (alpha-char-p (char token 0))
       (lower-case-p (char token 0))
       (find #\- token)
       (every (lambda (ch)
                (or (and (alpha-char-p ch) (lower-case-p ch))
                    (digit-char-p ch)
                    (find ch ".-_")))
              token)
       t))

(-> antigravity--parse-models-output (string) list)
(defun antigravity--parse-models-output (text)
  "Parse the stdout of `agy models` into provider model property lists."
  (let ((discovered nil)
        (seen       (make-hash-table :test #'equal)))
    (dolist (raw-line (uiop:split-string text :separator '(#\Newline)))
      (let* ((cleaned (antigravity--clean-terminal-line raw-line))
             (line    (if (or (uiop:string-prefix-p "- " cleaned)
                              (uiop:string-prefix-p "* " cleaned))
                          (string-trim '(#\Space #\Tab) (subseq cleaned 2))
                          cleaned)))
        (when (plusp (length line))
          (let* ((space-pos (position-if (lambda (ch) (or (char= ch #\Space) (char= ch #\Tab)))
                                         line))
                 (model-id  (if space-pos (subseq line 0 space-pos) line))
                 (label     (if space-pos
                                (string-trim '(#\Space #\Tab) (subseq line space-pos))
                                "")))
            (when (and (antigravity--model-token-p model-id)
                       (not (gethash model-id seen)))
              (setf (gethash model-id seen) t)
              (push (list :name              model-id
                          :description       label
                          :context-window    (antigravity--default-window-for model-id)
                          :reasoning-efforts '("none" "low" "medium" "high" "xhigh" "max" "ultra"))
                    discovered))))))
    (nreverse discovered)))

(-> antigravity--run-short-command (list &key (:seconds (integer 1)))
    (values string string integer))
(defun antigravity--run-short-command (argv &key (seconds 30))
  "Run ARGV non-interactively under a SECONDS deadline and return stdout, stderr, and exit code."
  (let ((process nil))
    (unwind-protect
         (provider-call-with-response-deadline
          seconds
          (lambda ()
            (setf process
                  (uiop:launch-program argv
                                       :input        nil
                                       :output       :stream
                                       :error-output :stream))
            (let ((stdout
                    (with-output-to-string (out)
                      (loop with stream = (uiop:process-info-output process)
                            for line = (read-line stream nil nil)
                            while line
                            do (write-line line out))))
                  (stderr
                    (with-output-to-string (out)
                      (loop with stream = (uiop:process-info-error-output process)
                            for line = (read-line stream nil nil)
                            while line
                            do (write-line line out))))
                  (code (platform-wait-process *platform* process)))
              (values stdout stderr (or code 1)))))
      (when process
        (when (uiop:process-alive-p process)
          (platform-terminate-process-object *platform* process :force t)
          (platform-wait-process *platform* process))
        (supervised-process--close-streams process)))))

(-> antigravity--fetch-models (configuration) list)
(defun antigravity--fetch-models (configuration)
  "Discover available models by invoking `agy models`."
  (declare (ignore configuration))
  (let ((command (antigravity-command)))
    (unless command
      (error 'cl-llm-provider-api:provider-model-discovery-error
             :provider-name "antigravity"
             :message       "The Antigravity CLI (agy) is not installed."))
    (handler-case
        (multiple-value-bind (stdout stderr exit-code)
            (antigravity--run-short-command (list command "models") :seconds 30)
          (unless (zerop exit-code)
            (error 'cl-llm-provider-api:provider-model-discovery-error
                   :provider-name "antigravity"
                   :message
                   (format nil "agy models exited with status ~D~@[: ~A~]"
                           exit-code
                           (provider--authentication-detail
                            (if (non-empty-string-p stderr) stderr stdout)))))
          (let ((models (antigravity--parse-models-output stdout)))
            (unless models
              (error 'cl-llm-provider-api:provider-model-discovery-error
                     :provider-name "antigravity"
                     :message       "agy models returned no recognizable model identifiers."))
            models))
      (cl-llm-provider-api:provider-model-discovery-error (condition)
        (error condition))
      (error (cause)
        (error 'cl-llm-provider-api:provider-model-discovery-error
               :provider-name "antigravity"
               :message       "Could not run `agy models`."
               :cause         cause)))))

(-> antigravity-provider-authenticate
    (model-provider &key (:stream (option stream)) (:open-browser-p boolean))
    string)
(defun antigravity-provider-authenticate (provider &key stream open-browser-p)
  "Authenticate the Antigravity CLI via `agy acp --login` or verify existing login."
  (declare (ignore provider))
  (let ((command (antigravity-command))
        (out     (or stream *standard-output*)))
    (unless command
      (error 'authentication-error
             :message
             "The Antigravity CLI (agy) was not found; install agy or set AUTOLITH_ANTIGRAVITY_COMMAND."))
    (if open-browser-p
        (let ((exit-code
                (nth-value
                 2
                 (uiop:run-program (list command "acp" "--login")
                                   :input               :interactive
                                   :output              out
                                   :error-output        out
                                   :ignore-error-status t))))
          (unless (eql exit-code 0)
            (error 'authentication-error
                   :message
                   (format nil "`agy acp --login` exited with status ~A." exit-code)))
          "Antigravity authentication succeeded via `agy acp --login`.")
        (multiple-value-bind (stdout stderr exit-code)
            (antigravity--run-short-command (list command "models") :seconds 30)
          (unless (and (zerop exit-code)
                       (antigravity--parse-models-output stdout))
            (error 'authentication-error
                   :message
                   (format nil
                           "Antigravity CLI is not authenticated~@[: ~A~]. Run `agy acp --login` in a terminal."
                           (provider--authentication-detail
                            (if (non-empty-string-p stderr) stderr stdout)))))
          "Antigravity CLI authentication is active."))))


;;;; -- Request Projection and Prompt Encoding --

(defmethod provider-wire-tools
    ((provider antigravity-provider) (tool-namespaces vector))
  "Flatten namespaced Autolith tools into canonical dotted tool specifications."
  (declare (ignore provider))
  (let ((flattened nil))
    (loop for entry across tool-namespaces
          when (json-object-p entry)
            do (let ((type (json-get entry "type")))
                 (cond
                   ((json-string= type "namespace")
                    (let ((namespace (json-get entry "name"))
                          (tools     (json-get entry "tools")))
                      (when (and (non-empty-string-p namespace)
                                 (vectorp tools)
                                 (not (stringp tools)))
                        (loop for tool across tools
                              when (and (json-object-p tool)
                                        (non-empty-string-p (json-get tool "name")))
                                do (push (json-object
                                          "name" (format nil "~A.~A"
                                                         namespace
                                                         (json-get tool "name"))
                                          "namespace" namespace
                                          "local_name" (json-get tool "name")
                                          "description" (or (json-get tool "description") "")
                                          "parameters" (or (json-get tool "parameters")
                                                           (json-object "type" "object")))
                                         flattened)))))
                   ((and (json-string= type "function")
                         (non-empty-string-p (json-get entry "name")))
                    (push (json-object
                           "name" (json-get entry "name")
                           "description" (or (json-get entry "description") "")
                           "parameters" (or (json-get entry "parameters")
                                            (json-object "type" "object")))
                          flattened)))))
    (coerce (nreverse flattened) 'vector)))

(defmethod provider-wire-input-item ((provider antigravity-provider) item)
  "Keep structured conversation items intact for Antigravity prompt rendering."
  (declare (ignore provider))
  item)

(defmethod provider-request-with-output-ceiling
    ((provider antigravity-provider) (request hash-table) (ceiling integer))
  "Record the mission output ceiling on a copy of REQUEST."
  (declare (ignore provider))
  (provider-request--with-output-field request "max_output_tokens" ceiling))

(-> antigravity--wire-effort (string (option string)) (option string))
(defun antigravity--wire-effort (model effort)
  "Return the `--effort` flag value for MODEL and EFFORT, or NIL when MODEL pins its own effort."
  (unless (some (lambda (suffix) (uiop:string-suffix-p model suffix))
                *antigravity-effort-suffixes*)
    (cond
      ((or (null effort) (string= effort "none"))
       nil)
      ((string= effort "ultra")
       "max")
      ((member effort '("low" "medium" "high" "xhigh" "max") :test #'string=)
       effort)
      (t
       nil))))

(defmethod provider-request-object
    ((provider antigravity-provider) (conversation conversation)
     (tool-namespaces vector) &key goal-context compaction-p)
  "Project product prompts, tools, and history into an Antigravity wire request."
  (let* ((configuration      (provider-configuration provider))
         (request-namespaces
           (if compaction-p
               #()
               (provider-request-tool-namespaces configuration tool-namespaces
                                                 :hosted-web-search-p t)))
         (effective-tools    (provider-wire-tools provider request-namespaces))
         (delivery
           (unless compaction-p
             (context-resolve-request configuration conversation request-namespaces
                                      :goal-context goal-context)))
         (durable-items
           (conversation-input-items-for-family
            conversation (provider-family provider) :include-ephemeral-p nil))
         (ephemeral-items
           (unless compaction-p
             (remove-if
              (lambda (item) (member item durable-items :test #'eq))
              (conversation-input-items-for-family
               conversation (provider-family provider) :include-ephemeral-p t))))
         (projection
           (multiple-value-bind (shared items)
               (provider-shared-items-split
                (context-delivery-project-items delivery conversation durable-items)
                durable-items)
             (make-instance
              'cl-llm-provider-api:wire-request
              :model   (config :model configuration)
              :items   items
              :prefix  (append
                        (list (let ((*system-prompt-hosted-web-search-p* nil))
                                (system-prompt configuration)))
                        (when compaction-p (list *compaction-instructions*)))
              :suffix  (remove-if-not
                        #'non-empty-string-p
                        (list (and (not compaction-p) goal-context)
                              (context-delivery-rendered-tail delivery)))
              :options (list :shared-items          shared
                             :ephemeral-items       ephemeral-items
                             :tool-choice           (if compaction-p
                                                        ':none
                                                        *provider-tool-choice*)
                             :reasoning-effort      (config :reasoning-effort configuration)
                             :maximum-output-tokens *provider-maximum-output-tokens*)))))
    (values (provider-request-object provider projection effective-tools
                                     :compaction-p compaction-p)
            delivery)))

(-> antigravity--context-item (string) json-object)
(defun antigravity--context-item (text)
  "Wrap suffix context TEXT as a developer message item."
  (json-object
   "type" "message"
   "role" "developer"
   "content" (json-array (json-object "type" "input_text" "text" text))))

(defmethod provider-request-object
    ((provider antigravity-provider) (projection cl-llm-provider-api:wire-request)
     (tool-namespaces vector) &key goal-context compaction-p)
  "Encode PROJECTION and TOOL-NAMESPACES into a canonical JSON request object."
  (declare (ignore provider goal-context))
  (let* ((options         (cl-llm-provider-api:wire-request-options projection))
         (model           (cl-llm-provider-api:wire-request-model projection))
         (shared-items    (getf options :shared-items))
         (ephemeral-items (getf options :ephemeral-items))
         (suffix-items    (mapcar #'antigravity--context-item
                                  (remove-if-not
                                   #'non-empty-string-p
                                   (cl-llm-provider-api:wire-request-suffix projection))))
         (all-items       (append shared-items
                                  (cl-llm-provider-api:wire-request-items projection)
                                  ephemeral-items
                                  suffix-items))
         (volatile-count  (+ (length ephemeral-items) (length suffix-items)))
         (tool-choice     (if (or compaction-p (eq (getf options :tool-choice) ':none))
                              "none"
                              "auto"))
         (effort          (antigravity--wire-effort
                           model (getf options :reasoning-effort)))
         (ceiling         (getf options :maximum-output-tokens))
         (instructions    (format nil "~{~A~^~2%~}"
                                  (remove-if-not
                                   #'non-empty-string-p
                                   (cl-llm-provider-api:wire-request-prefix projection))))
         (request         (json-object
                           "model" model
                           "instructions" instructions
                           "input" (coerce all-items 'vector)
                           "tools" tool-namespaces
                           "tool_choice" tool-choice)))
    (when effort
      (setf (gethash "effort" request) effort))
    (when (and (integerp ceiling) (plusp ceiling))
      (setf (gethash "max_output_tokens" request) ceiling))
    (values request volatile-count)))

(-> antigravity--turn-schema-json (boolean) string)
(defun antigravity--turn-schema-json (allow-tools-p)
  "Return the JSON Schema enforced on `agy` structured output."
  (let ((properties
          (if allow-tools-p
              (json-object
               "text" (json-object
                       "type" "string"
                       "description" "Visible assistant text for the user (may be empty when calling host tools).")
               "tool_calls" (json-object
                             "type" "array"
                             "description" "Autolith host tool calls to execute in order."
                             "items" (json-object
                                      "type" "object"
                                      "properties" (json-object
                                                    "name" (json-object
                                                            "type" "string"
                                                            "description" "Exact host tool name from HOST TOOLS.")
                                                    "arguments" (json-object
                                                                 "type" "string"
                                                                 "description" "JSON-encoded object matching the tool's parameters schema."))
                                      "required" (json-array "name" "arguments")
                                      "additionalProperties" (json-false))))
              (json-object
               "text" (json-object
                       "type" "string"
                       "description" "Complete visible assistant response.")))))
    (json-encode
     (json-object
      "type" "object"
      "properties" properties
      "required" (if allow-tools-p
                     (json-array "text" "tool_calls")
                     (json-array "text"))
      "additionalProperties" (json-false)))))

(-> antigravity--render-message-parts (t) string)
(defun antigravity--render-message-parts (content)
  "Render message CONTENT parts as readable prompt text."
  (cond
    ((stringp content)
     content)
    ((and (vectorp content) (not (stringp content)))
     (with-output-to-string (out)
       (loop for part across content
             for index from 0
             do (when (plusp index) (terpri out))
                (cond
                  ((stringp part)
                   (write-string part out))
                  ((json-object-p part)
                   (let ((part-type (json-get part "type"))
                         (text      (json-get part "text")))
                     (cond
                       ((stringp text)
                        (write-string text out))
                       ((member part-type '("input_image" "image" "image_url") :test #'equal)
                        (write-string "[image attachment]" out))
                       (t
                        (write-string (json-encode part) out)))))
                  (t
                   (princ part out))))))
    (t
     "")))

(-> antigravity--render-item (t stream) null)
(defun antigravity--render-item (item stream)
  "Append one projected conversation ITEM to STREAM."
  (when (json-object-p item)
    (let ((type (json-get item "type")))
      (cond
        ((equal type "message")
         (let ((role (string-upcase (or (json-get item "role") "user")))
               (body (antigravity--render-message-parts (json-get item "content"))))
           (format stream "[~A]~%~A~2%" role body)))
        ((equal type "function_call")
         (let* ((namespace (json-get item "namespace"))
                (name      (or (json-get item "name") ""))
                (full-name (if (non-empty-string-p namespace)
                               (format nil "~A.~A" namespace name)
                               name))
                (call-id   (or (json-get item "call_id") ""))
                (arguments (or (json-get item "arguments") "{}")))
           (format stream "[ASSISTANT_TOOL_CALL id=~A name=~A]~%~A~2%"
                   call-id full-name arguments)))
        ((equal type "function_call_output")
         (let* ((call-id (or (json-get item "call_id") ""))
                (output  (json-get item "output"))
                (text    (cond
                           ((stringp output) output)
                           ((null output) "")
                           (t (json-encode output)))))
           (format stream "[TOOL_OUTPUT id=~A]~%~A~2%" call-id text)))
        ((equal type "reasoning")
         (let ((summary (response-item-reasoning-summary item)))
           (when (non-empty-string-p summary)
             (format stream "[ASSISTANT_REASONING]~%~A~2%" summary)))))))
  nil)

(-> antigravity--render-prompt (hash-table boolean) string)
(defun antigravity--render-prompt (request allow-tools-p)
  "Render REQUEST into a self-contained structured prompt for the Antigravity CLI."
  (let ((instructions (json-get request "instructions"))
        (tools        (json-get request "tools"))
        (items        (json-get request "input"))
        (ceiling      (json-get request "max_output_tokens")))
    (with-output-to-string (out)
      (write-line "=== AUTOLITH INFERENCE ENVELOPE ===" out)
      (write-line "You are serving as the language model backend for an external Common Lisp agent host (Autolith)." out)
      (write-line "CRITICAL TRANSPORT RULES:" out)
      (write-line "1. Do NOT invoke any native Antigravity CLI, IDE, browser, shell, file, or MCP tools (such as run_command, view_file, write_to_file, replace_file_content, call_mcp_tool, search_web, or invoke_subagent), except reading prompt.txt if your user prompt explicitly instructs you to read prompt.txt." out)
      (if allow-tools-p
          (progn
            (write-line "2. All tool execution is owned by Autolith. Whenever you want to call one or more tools, list them in your final JSON output's \"tool_calls\" array using the exact tool \"name\" from HOST TOOLS below, and encode \"arguments\" as a JSON object string (for example \"{\\\"code\\\":\\\"(+ 2 2)\\\"}\")." out)
            (write-line "3. When no tool call is needed, return an empty \"tool_calls\": [] array and place ONLY your plain/markdown reply text in \"text\" (never serialize a nested JSON envelope inside \"text\")." out))
          (write-line "2. Tool calls are disabled for this request. Return ONLY your plain/markdown reply text in the JSON \"text\" field without calling any tools." out))
      (when (and (integerp ceiling) (plusp ceiling))
        (format out "4. Keep your response concise and within approximately ~D output tokens.~%" ceiling))
      (terpri out)
      (when (non-empty-string-p instructions)
        (write-line "=== SYSTEM INSTRUCTIONS ===" out)
        (write-string instructions out)
        (terpri out)
        (terpri out))
      (when (and allow-tools-p (vectorp tools) (plusp (length tools)))
        (write-line "=== HOST TOOLS ===" out)
        (loop for tool across tools
              when (json-object-p tool)
                do (format out "- ~A: ~A~%  parameters: ~A~%"
                           (json-get tool "name")
                           (or (json-get tool "description") "")
                           (json-encode (or (json-get tool "parameters")
                                            (json-object "type" "object")))))
        (terpri out))
      (write-line "=== CONVERSATION TRANSCRIPT ===" out)
      (when (and (vectorp items) (not (stringp items)))
        (loop for item across items
              do (antigravity--render-item item out)))
      (write-line "=== END OF TRANSCRIPT ===" out)
      (write-line "Respond to the latest turn in the conversation transcript above." out))))


;;;; -- Subprocess Streaming Execution --

(-> antigravity--scratch-directory (configuration) pathname)
(defun antigravity--scratch-directory (configuration)
  "Create an isolated empty scratch directory with a `.git` boundary marker."
  (let* ((root      (or (and configuration
                             (handler-case (config :cache-root configuration)
                               (error () nil)))
                        (uiop:temporary-directory)))
         (directory (uiop:ensure-directory-pathname
                     (merge-pathnames
                      (format nil "antigravity-ws/~A/" (make-identifier))
                      root)))
         (git-dir   (merge-pathnames ".git/" directory)))
    (ensure-directories-exist git-dir)
    #-win32
    (handler-case (sb-posix:chmod (namestring directory) #o700)
      (error () nil))
    directory))

(-> antigravity--write-private-file (pathname string) pathname)
(defun antigravity--write-private-file (pathname content)
  "Write UTF-8 CONTENT to PATHNAME with private permissions."
  (ensure-directories-exist pathname)
  (with-open-file (out pathname
                       :direction         :output
                       :if-exists         :supersede
                       :if-does-not-exist :create
                       :external-format   :utf-8)
    (write-string content out))
  #-win32
  (handler-case (sb-posix:chmod (namestring pathname) #o600)
    (error () nil))
  pathname)

(-> antigravity--cleanup-scratch-directory (pathname) null)
(defun antigravity--cleanup-scratch-directory (directory)
  "Remove temporary Antigravity scratch DIRECTORY safely."
  (handler-case
      (uiop:delete-directory-tree directory
                                  :validate          t
                                  :if-does-not-exist :ignore)
    (error ()
      nil))
  nil)

(-> antigravity--normalize-usage (t) (option json-object))
(defun antigravity--normalize-usage (usage)
  "Map `agy` usage counters onto Autolith's portable provider usage schema."
  (when (json-object-p usage)
    (let ((normalized (json-object))
          (input      (json-get usage "input_tokens"))
          (output     (json-get usage "output_tokens"))
          (thinking   (json-get usage "thinking_tokens"))
          (cached     (json-get usage "cache_read_tokens"))
          (total      (json-get usage "total_tokens")))
      (when (typep input '(integer 0))
        (setf (gethash "input_tokens" normalized) input))
      (when (typep output '(integer 0))
        (setf (gethash "output_tokens" normalized)
              (if (typep thinking '(integer 1))
                  (+ output thinking)
                  output)))
      (when (typep thinking '(integer 0))
        (setf (gethash "reasoning_tokens" normalized) thinking))
      (when (typep cached '(integer 0))
        (setf (gethash "cached_input_tokens" normalized) cached
              (gethash "cache_read_input_tokens" normalized) cached))
      (when (typep total '(integer 0))
        (setf (gethash "total_tokens" normalized) total))
      (provider-usage-normalize normalized))))

(-> antigravity--resolve-tool-call (json-object vector) json-object)
(defun antigravity--resolve-tool-call (raw-call offered-tools)
  "Convert one `agy` structured tool_call object into an Autolith function_call item."
  (let* ((raw-name  (or (json-get raw-call "name") ""))
         (raw-args  (json-get raw-call "arguments"))
         (arguments (cond
                      ((stringp raw-args) raw-args)
                      ((null raw-args) "{}")
                      (t (json-encode raw-args))))
         (matched   (when (and (vectorp offered-tools) (not (stringp offered-tools)))
                      (find raw-name offered-tools
                            :key  (lambda (tool)
                                    (and (json-object-p tool) (json-get tool "name")))
                            :test #'equal)))
         (namespace (and matched (json-get matched "namespace")))
         (local     (and matched (json-get matched "local_name"))))
    (unless (and (non-empty-string-p namespace) (non-empty-string-p local))
      (multiple-value-bind (decoded-ns decoded-name)
          (provider-wire-function-name--decode raw-name)
        (if (and decoded-ns decoded-name)
            (setf namespace decoded-ns
                  local     decoded-name)
            (let ((dot (position #\. raw-name)))
              (if (and dot (plusp dot) (< (1+ dot) (length raw-name)))
                  (setf namespace (subseq raw-name 0 dot)
                        local     (subseq raw-name (1+ dot)))
                  (setf namespace nil
                        local     raw-name))))))
    (let ((item (json-object
                 "type" "function_call"
                 "call_id" (format nil "call_~A" (make-identifier))
                 "name" local
                 "arguments" arguments)))
      (when (non-empty-string-p namespace)
        (setf (gethash "namespace" item) namespace))
      item)))

(-> antigravity--signal-cli-failure
    (antigravity-provider string &key (:response-id (option string)) (:exit-code (option integer)))
    null)
(defun antigravity--signal-cli-failure (provider detail &key response-id exit-code)
  "Classify an `agy` CLI error into unauthorized, retryable, or terminal provider-error."
  (declare (ignore provider))
  (let* ((clean   (or (provider--authentication-detail detail :limit 1000)
                      (format nil "Antigravity CLI failed~@[ (exit code ~D)~]." exit-code)))
         (lower   (string-downcase clean)))
    (cond
      ((or (search "not logged in" lower)
           (search "unauthenticated" lower)
           (search "acp --login" lower)
           (search "unauthorized" lower)
           (search "authentication" lower))
       (error 'provider-unauthorized
              :message     clean
              :status      401
              :response-id response-id
              :response    nil))
      ((or (search "rate limit" lower)
           (search "resource_exhausted" lower)
           (search "429" lower)
           (search "unavailable" lower)
           (search "overloaded" lower)
           (search "deadline_exceeded" lower))
       (error 'provider-retryable-error
              :message     clean
              :status      (when (search "429" lower) 429)
              :code        (when (search "resource_exhausted" lower) "RESOURCE_EXHAUSTED")
              :response-id response-id
              :response    clean))
      (t
       (error 'provider-error
              :message     clean
              :status      nil
              :response-id response-id
              :response    clean)))))

(-> antigravity--unwrap-structured-payload (json-object) json-object)
(defun antigravity--unwrap-structured-payload (payload)
  "Unwrap PAYLOAD when `agy --json-schema` wraps an already-encoded envelope into `text`."
  (let* ((outer-calls (json-get payload "tool_calls"))
         (empty-calls (or (null outer-calls)
                          (and (vectorp outer-calls)
                               (not (stringp outer-calls))
                               (zerop (length outer-calls)))))
         (text        (json-get payload "text"))
         (trimmed     (and empty-calls
                           (stringp text)
                           (string-trim '(#\Space #\Tab #\Newline #\Return) text))))
    (if (and (non-empty-string-p trimmed)
             (char= (char trimmed 0) #\{))
        (let ((inner (handler-case (json-decode trimmed)
                       (error () nil))))
          (if (and (json-object-p inner)
                   (stringp (json-get inner "text"))
                   (loop for key being the hash-keys of inner
                         always (or (string= key "text")
                                    (string= key "tool_calls"))))
              inner
              payload))
        payload)))

(-> antigravity--execute-cli-request
    (antigravity-provider string hash-table function)
    provider-result)
(defun antigravity--execute-cli-request (provider command request event-callback)
  "Execute encoded REQUEST through `agy --output-format stream-json` and return a PROVIDER-RESULT."
  (let* ((configuration   (provider-configuration provider))
         (model           (json-get request "model"))
         (effort          (json-get request "effort"))
         (tools           (json-get request "tools"))
         (allow-tools-p   (and (not (equal (json-get request "tool_choice") "none"))
                               (vectorp tools)
                               (plusp (length tools))
                               t))
         (prompt          (antigravity--render-prompt request allow-tools-p))
         (schema-json     (antigravity--turn-schema-json allow-tools-p))
         (scratch-dir     (antigravity--scratch-directory configuration))
         (schema-file     (antigravity--write-private-file
                           (merge-pathnames "schema.json" scratch-dir)
                           schema-json))
         (cli-prompt
           (if (<= (length prompt) *antigravity-inline-prompt-limit*)
               prompt
               (progn
                 (antigravity--write-private-file
                  (merge-pathnames "prompt.txt" scratch-dir)
                  prompt)
                 "Read the file prompt.txt in the current directory in full using view_file, then respond immediately with the required JSON object without invoking any other native tools.")))
         (argv
           (append (list command
                         "--output-format" "stream-json"
                         "--disable-slash-commands"
                         "--sandbox")
                   (when (non-empty-string-p model)
                     (list "--model" model))
                   (when (non-empty-string-p effort)
                     (list "--effort" effort))
                   (list "--json-schema" (namestring schema-file)
                         (concatenate 'string "--print=" cli-prompt))))
         (process         nil)
         (response-id     nil)
         (observed-model  model)
         (usage           nil)
         (result-event    nil)
         (diagnostic-tail nil))
    (unwind-protect
         (progn
           (setf process
                 (uiop:launch-program argv
                                      :directory    scratch-dir
                                      :input        nil
                                      :output       :stream
                                      :error-output :stream))
           (loop with stdout = (uiop:process-info-output process)
                 for raw-line = (read-line stdout nil nil)
                 while raw-line
                 do (let ((trimmed (string-trim '(#\Space #\Tab #\Return #\Newline) raw-line)))
                      (when (plusp (length trimmed))
                        (if (char= (char trimmed 0) #\{)
                            (let ((decoded (handler-case (json-decode trimmed)
                                             (error () nil))))
                              (when (json-object-p decoded)
                                (let ((event-kind (json-get decoded "event")))
                                  (cond
                                    ((json-string= event-kind "init")
                                     (let ((cid  (json-get decoded "conversation_id"))
                                           (init (json-get decoded "init")))
                                       (when (non-empty-string-p cid)
                                         (setf response-id cid))
                                       (when (and (json-object-p init)
                                                  (non-empty-string-p (json-get init "model")))
                                         (setf observed-model (json-get init "model")))))
                                    ((json-string= event-kind "step_update")
                                     (let ((step (json-get decoded "step_update")))
                                       (when (json-object-p step)
                                         (let ((thinking-delta (json-get step "thinking_delta"))
                                               (step-usage     (json-get step "usage")))
                                           (when (json-object-p step-usage)
                                             (setf usage (antigravity--normalize-usage step-usage)))
                                           (cond
                                             ((and (stringp thinking-delta)
                                                   (plusp (length thinking-delta)))
                                              (funcall event-callback
                                                       (if (provider-reasoning-summaries-p provider)
                                                            (make-instance 'reasoning-delta-event
                                                                           :text thinking-delta)
                                                            (make-instance 'cl-llm-provider-api:generation-progress-event))))
                                             ((json-string= (json-get step "step_type")
                                                            "agent_response")
                                              (funcall event-callback
                                                       (make-instance 'cl-llm-provider-api:generation-progress-event))))))))
                                    ((json-string= event-kind "result")
                                     (let ((res (json-get decoded "result")))
                                       (when (json-object-p res)
                                         (setf result-event res)
                                         (when (non-empty-string-p (json-get res "conversation_id"))
                                           (setf response-id (json-get res "conversation_id")))
                                         (when (json-object-p (json-get res "usage"))
                                           (setf usage
                                                 (antigravity--normalize-usage
                                                  (json-get res "usage")))))))))))
                            (unless (uiop:string-prefix-p "warning:" trimmed)
                              (push trimmed diagnostic-tail))))))
           (let* ((stderr
                    (with-output-to-string (out)
                      (loop with stream = (uiop:process-info-error-output process)
                            for line = (read-line stream nil nil)
                            while line
                            do (let ((trimmed (string-trim '(#\Space #\Tab #\Return) line)))
                                 (unless (uiop:string-prefix-p "warning:" trimmed)
                                   (write-line trimmed out))))))
                  (exit-code (platform-wait-process *platform* process)))
             (unless (and (eql exit-code 0)
                          (json-object-p result-event)
                          (json-string= (json-get result-event "status") "SUCCESS"))
               (antigravity--signal-cli-failure
                provider
                (or (and (json-object-p result-event)
                         (non-empty-string-p (json-get result-event "error"))
                         (json-get result-event "error"))
                    (and (non-empty-string-p stderr) stderr)
                    (format nil "~{~A~^ ~}" (nreverse diagnostic-tail)))
                :response-id response-id
                :exit-code   exit-code))
             (let* ((structured   (json-get result-event "structured_output"))
                    (raw-response (json-get result-event "response"))
                    (parsed-json
                      (cond
                        ((json-object-p structured)
                         structured)
                        ((non-empty-string-p raw-response)
                         (handler-case (json-decode raw-response)
                           (error () nil)))
                        (t
                         nil))))
               (unless (json-object-p parsed-json)
                 (antigravity--signal-cli-failure
                  provider
                  (format nil "Antigravity CLI completed without structured JSON output~@[ (~A)~]."
                          (and (json-object-p result-event)
                               (json-get result-event "denied_actions")
                               "native tool action was denied; host tools must be returned in tool_calls"))
                  :response-id response-id
                  :exit-code   exit-code))
               (let* ((payload (antigravity--unwrap-structured-payload parsed-json))
                      (text
                        (let ((val (json-get payload "text")))
                          (if (stringp val) val "")))
                      (raw-calls
                        (when allow-tools-p
                          (let ((val (json-get payload "tool_calls")))
                            (cond
                              ((and (vectorp val) (not (stringp val)))
                               (coerce val 'list))
                              ((listp val)
                               val)
                              (t
                               nil)))))
                      (call-items
                        (loop for raw-call in raw-calls
                              when (json-object-p raw-call)
                                collect (antigravity--resolve-tool-call raw-call tools)))
                      (output-items
                        (append
                         (when (or (plusp (length text)) (null call-items))
                           (list (json-object
                                  "type" "message"
                                  "role" "assistant"
                                  "content" (json-array
                                             (json-object "type" "output_text"
                                                          "text" text)))))
                         call-items))
                      (final-usage
                        (or usage
                            (json-object "input_tokens" 0
                                         "output_tokens" 0
                                         "total_tokens" 0)))
                      (metadata
                        (make-instance 'provider-response-metadata
                                       :model             observed-model
                                       :finish-reason     (if call-items "tool_calls" "stop")
                                       :usage-observed-p  (not (null usage))
                                       :reasoning-context nil))
                      (final-id (or response-id (make-identifier))))
                 (funcall event-callback
                          (make-instance 'provider-response-metadata-event
                                         :metadata metadata))
                 (when (plusp (length text))
                   (funcall event-callback
                            (make-instance 'assistant-delta-event :text text)))
                 (dolist (item output-items)
                   (funcall event-callback
                            (make-instance 'provider-item-event :item item)))
                 (funcall event-callback
                          (make-instance 'provider-completed-event
                                         :response-id     final-id
                                         :usage           final-usage
                                         :metadata        metadata
                                         :turn-completion (if call-items
                                                              ':continue
                                                              ':end)))
                 (make-instance 'provider-result
                                :response-id     final-id
                                :output-items    output-items
                                :tool-calls      call-items
                                :usage           final-usage
                                :metadata        metadata
                                :turn-state      nil
                                :turn-completion (if call-items
                                                     ':continue
                                                     ':end))))))
      (when process
        (when (uiop:process-alive-p process)
          (platform-terminate-process-object *platform* process :force t)
          (platform-wait-process *platform* process))
        (supervised-process--close-streams process))
      (antigravity--cleanup-scratch-directory scratch-dir))))

(defmethod provider-attempt-turn
    ((provider antigravity-provider) (conversation conversation)
     &key tool-namespaces event-callback force-refresh goal-context compaction-p)
  "Execute one Antigravity CLI turn within Autolith's context-admission and delivery protocol."
  (with-credentials (credentials (provider-credential-manager provider)
                                 :force-refresh force-refresh)
    (let ((command (getf credentials :command)))
      (multiple-value-bind (request delivery)
          (provider-request-object provider conversation tool-namespaces
                                   :goal-context goal-context
                                   :compaction-p compaction-p)
        (provider-call-with-mission-output-allowance
         provider request
         (lambda (final-request)
           (provider-context--admit-request provider final-request)
           (when (and conversation *provider-context-request*
                      (eq conversation (getf *provider-context-request* :conversation)))
             (setf (getf *provider-context-request* :wire-provider) provider
                   (getf *provider-context-request* :wire)
                   (provider-context--wire-plan final-request)))
           (provider-consume-compaction-recovery-permit provider)
           (when *provider-context-admission-state*
             (setf (first *provider-context-admission-state*) :entered
                   (rest *provider-context-admission-state*) nil))
           (provider-cache-prefix-observe final-request)
           (when (and delivery event-callback)
             (context-delivery-notify delivery event-callback))
           (multiple-value-prog1
               (antigravity--execute-cli-request
                provider command final-request event-callback)
             (when delivery
               (context-delivery-complete delivery)))))))))
