(in-package #:autolith)

;;;; -- Prompt-Cache Miss Tests --

(-> prompt-cache-tests--usage (integer (option integer)) list)
(defun prompt-cache-tests--usage (input cached)
  "Return portable usage with INPUT prompt tokens and optional CACHED reads."
  (append (list (list "input_tokens" input)
                (list "output_tokens" 10))
          (and cached
               (list (list "cached_input_tokens" cached)))))

(-> test-prompt-cache-miss-detection () null)
(defun test-prompt-cache-miss-detection ()
  "Test cache-miss thresholds, re-read accounting, and cause attribution."
  (let ((baseline (make-instance 'prompt-cache-baseline
                                 :prompt-tokens 40000
                                 :completed-at 1000
                                 :model "model-a")))
    (test-assert
     (null (prompt-cache-miss-detect nil (prompt-cache-tests--usage 41000 0)
                                     :started-at 1010
                                     :model "model-a"))
     "the first request of a conversation has no baseline to miss")
    (test-assert
     (null (prompt-cache-miss-detect baseline
                                     (prompt-cache-tests--usage 41000 39000)
                                     :started-at 1010
                                     :model "model-a"))
     "a request served mostly from the cache is not a miss")
    (test-assert
     (null (prompt-cache-miss-detect baseline
                                     (prompt-cache-tests--usage 41000 20000)
                                     :started-at 1010
                                     :model "model-a"))
     "cache reads reaching half of the previous prompt are not a miss")
    (test-assert
     (null (prompt-cache-miss-detect baseline
                                     (prompt-cache-tests--usage 41000 nil)
                                     :started-at 1010
                                     :model "model-a"))
     "providers without cache counters report nothing")
    (test-assert
     (null (prompt-cache-miss-detect
            (make-instance 'prompt-cache-baseline
                           :prompt-tokens 500
                           :completed-at 1000
                           :model "model-a")
            (prompt-cache-tests--usage 600 0)
            :started-at 1010
            :model "model-a"))
     "prompts below the cacheable minimum report nothing")
    (dolist (case '((:model-changed 1010 "model-b" 41000 0 40000 10)
                    (:idle 1400 "model-a" 41000 100 39900 400)
                    (:context-rewritten 1010 "model-a" 8000 0 8000 10)
                    (:prefix-changed 1010 "model-a" 41000 19999 20001 10)))
      (destructuring-bind (cause started-at model input cached re-read idle)
          case
        (let ((miss (prompt-cache-miss-detect
                     baseline
                     (prompt-cache-tests--usage input cached)
                     :started-at started-at
                     :model model)))
          (test-assert
           (and miss
                (eq (getf miss :cause) cause)
                (= (getf miss :re-read-tokens) re-read)
                (= (getf miss :prompt-tokens) input)
                (= (getf miss :cached-tokens) cached)
                (eql (getf miss :idle-seconds) idle))
           (format nil "a ~(~A~) miss reports its re-read tokens and cause"
                   cause)))))
    (let ((miss (prompt-cache-miss-detect
                 (make-instance 'prompt-cache-baseline :prompt-tokens 40000)
                 (prompt-cache-tests--usage 41000 0)
                 :started-at 1010
                 :model "model-a")))
      (test-assert
       (and miss
            (eq (getf miss :cause) ':resumed)
            (null (getf miss :idle-seconds)))
       "a persisted baseline without a time attributes the miss to resuming"))
    (let ((stalled (make-instance 'prompt-cache-baseline
                                  :prompt-tokens 72000
                                  :cached-tokens 63872
                                  :completed-at 1000
                                  :model "model-a")))
      (dolist (case '((73000 63872 t "reads that stay flat behind a growing prompt are a stall")
                      (73000 63900 t "reads within one cache block of the previous level still stall")
                      (73000 70000 nil "reads that grew with the prompt are healthy")
                      (73000 60000 t "reads that fell below the previous level also stall")
                      (71000 63872 nil "a shorter prompt is not a stall")))
        (destructuring-bind (input cached stall-p description) case
          (let ((miss (prompt-cache-miss-detect
                       stalled
                       (prompt-cache-tests--usage input cached)
                       :started-at 1010
                       :model "model-a")))
            (test-assert
             (if stall-p
                 (and miss
                      (eq (getf miss :cause) ':prefix-stalled)
                      (= (getf miss :re-read-tokens) (- 72000 cached)))
                 (or (null miss)
                     (not (eq (getf miss :cause) ':prefix-stalled))))
             description))))
      (test-assert
       (null (prompt-cache-miss-detect
              (make-instance 'prompt-cache-baseline
                             :prompt-tokens 72000
                             :cached-tokens 69000
                             :completed-at 1000
                             :model "model-a")
              (prompt-cache-tests--usage 73000 69000)
              :started-at 1010
              :model "model-a"))
       "a small re-read behind flat reads is the request-local tail, not a stall")
      (test-assert
       (null (prompt-cache-miss-detect
              (make-instance 'prompt-cache-baseline
                             :prompt-tokens 72000
                             :completed-at 1000
                             :model "model-a")
              (prompt-cache-tests--usage 73000 63872)
              :started-at 1010
              :model "model-a"))
       "a baseline without cache reads cannot judge a stall"))
    (let ((miss (prompt-cache-miss-detect
                 baseline
                 (append (prompt-cache-tests--usage 41000 0)
                         (list
                          (list "attribution"
                                (list
                                 (list "request_fields"
                                       (list
                                        (list "instructions"
                                              (list (list "cached_tokens" 5627)
                                                    (list "input_tokens" 5738)))
                                        (list "tools"
                                              (list (list "cached_tokens" 0)
                                                    (list "input_tokens" 9221)))))))))
                 :started-at 1010
                 :model "model-a")))
      (test-assert
       (and miss
            (= (getf miss :instructions-re-read) 111)
            (= (getf miss :tools-re-read) 9221))
       "server attribution reports uncached instructions and tools")))
  nil)

(-> test-prompt-cache-baseline-from-conversation () null)
(defun test-prompt-cache-baseline-from-conversation ()
  "Test the baseline seeded from a conversation's newest persisted usage."
  (with-test-configuration (configuration)
    (let ((conversation
            (conversation-create configuration :identifier "cache-baseline")))
      (test-assert
       (null (prompt-cache-baseline-from-conversation conversation))
       "an empty conversation seeds no baseline")
      (conversation-append-user-message conversation "first")
      (conversation-append-provider-metadata
       conversation
       (list :request-number 1 :usage (prompt-cache-tests--usage 1500 0)))
      (conversation-append-provider-metadata
       conversation
       (list :request-number 2 :usage (prompt-cache-tests--usage 3000 1400)))
      (conversation-append-provider-metadata
       conversation
       (list :request-number 3 :usage nil))
      (let ((baseline (prompt-cache-baseline-from-conversation conversation)))
        (test-assert
         (and baseline
              (= (prompt-cache-baseline-prompt-tokens baseline) 3000)
              (null (prompt-cache-baseline-completed-at baseline))
              (null (prompt-cache-baseline-model baseline)))
         "the newest usage with a prompt size seeds an untimed baseline"))))
  nil)

(-> test-prompt-cache-miss-notices () null)
(defun test-prompt-cache-miss-notices ()
  "Exercise compact colored cache-miss notices through the request lifecycle."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (conversation
           (conversation-create configuration :identifier "cache-miss-ui"))
         (terminal (make-instance 'recording-terminal :columns 40 :styled-p t))
         (ui (terminal-ui-create :terminal terminal))
         (application (make-instance 'application
                                     :configuration configuration
                                     :conversation conversation
                                     :ui ui)))
    (unwind-protect
         (let* ((observer (application-agent-observer application))
                (send-status
                  (callback-agent-observer-status-callback observer)))
           (flet ((notice-p ()
                    "Return whether the terminal painted a cache-miss notice."
                    (not (null (search "Cache miss"
                                       (clinedi:ansi-strip (recording-terminal-output terminal))))))

                  (notice-counts-p (re-read total)
                    "Return whether the rendered notice includes both token counts."
                    (let ((output (clinedi:ansi-strip (recording-terminal-output terminal))))
                      (and (search "Cache miss" output)
                           (search (application--token-count-description re-read) output)
                           (search (application--token-count-description total) output))))

                  (request (usage)
                    "Run one request lifecycle completing with USAGE."
                    (recording-terminal-reset terminal)
                    (funcall send-status :provider-request-started nil)
                    (funcall send-status
                             :provider-request-completed
                             (list :request-number 1 :usage usage))))
             (terminal-ui-start ui)
             (conversation-append-user-message conversation "history")
             (conversation-append-provider-metadata
              conversation
              (list :request-number 1
                    :usage (prompt-cache-tests--usage 30000 0)))
             (request (prompt-cache-tests--usage 31000 0))
             (test-assert (not (notice-p))
                          "disabled notices are omitted")
             (test-assert
              (= (prompt-cache-baseline-prompt-tokens
                  (application-prompt-cache-baseline application))
                 31000)
              "every completed request advances the baseline")
             (setf (application-cache-miss-notices-p application) t)
             (request (prompt-cache-tests--usage 32000 31500))
             (test-assert (not (notice-p))
                          "cache hits are not reported")
             (setf (application-prompt-cache-baseline application)
                   (make-instance 'prompt-cache-baseline
                                  :prompt-tokens 32000
                                  :completed-at (- (get-universal-time) 900)
                                  :model (config :model configuration)))
             (request (prompt-cache-tests--usage 33000 0))
             (test-assert (notice-counts-p 32000 33000)
                          "an idle miss reports re-read and total tokens")
             (let* ((spans (application--prompt-cache-miss-notice
                            '(:re-read-tokens 32000 :prompt-tokens 33000)))
                    (counts (remove-if-not
                             (lambda (span) (eq ':code (terminal-span-style span))) spans)))
               (test-assert (eq ':notice (terminal-span-style (first spans)))
                            "the cache-miss label has warning color")
               (test-assert (equal (mapcar #'terminal-span-text counts)
                                   (mapcar #'application--token-count-description '(32000 33000)))
                            "both token counts have a distinct color")
               (test-assert (<= (text-cell-width (terminal--spans-text spans)) 40)
                            "the notice fits a narrow transcript row"))
             (setf (application-prompt-cache-baseline application)
                   (make-instance 'prompt-cache-baseline
                                  :prompt-tokens 72000
                                  :cached-tokens 63872
                                  :completed-at (get-universal-time)
                                  :model (config :model configuration)))
             (request (append
                       (prompt-cache-tests--usage 73000 63872)
                       (list
                        (list "attribution"
                              (list
                               (list "request_fields"
                                     (list
                                      (list "instructions"
                                            (list (list "cached_tokens" 5738)
                                                  (list "input_tokens" 5738)))
                                      (list "tools"
                                            (list (list "cached_tokens" 9221)
                                                  (list "input_tokens" 9221))))))))))
             (test-assert (notice-counts-p 8128 73000)
                          "a stalled cache reports its re-read and total tokens")
             (funcall send-status :compaction-completed nil)
             (test-assert
              (null (application-prompt-cache-baseline application))
              "compaction drops the baseline its rewrite invalidated")
             (request (prompt-cache-tests--usage 9000 0))
             (test-assert (not (notice-p))
                          "the first request after compaction is expected")
             (let ((resumed
                     (conversation-create configuration
                                          :identifier "cache-miss-resumed")))
               (conversation-append-user-message resumed "older history")
               (conversation-append-provider-metadata
                resumed
                (list :request-number 1
                      :usage (prompt-cache-tests--usage 20000 19000)))
               (setf (application-conversation application) resumed)
               (request (prompt-cache-tests--usage 21000 0))
               (test-assert (notice-counts-p 20000 21000)
                            "switching conversations reseeds the baseline from its history"))))
      (ignore-errors (terminal-ui-stop ui))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-cache-misses-command () null)
(defun test-cache-misses-command ()
  "Test the /cache-misses command persists and applies its preference."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (conversation
           (conversation-create configuration :identifier "cache-miss-command"))
         (terminal (make-instance 'recording-terminal :columns 100))
         (ui (terminal-ui-create :terminal terminal))
         (application (make-instance 'application
                                     :configuration configuration
                                     :conversation conversation
                                     :ui ui)))
    (unwind-protect
         (progn
           (terminal-ui-start ui)
           (application-cache-miss-notices-command application t)
           (test-assert
            (and (application-cache-miss-notices-p application)
                 (config :cache-miss-notices-p configuration))
            "/cache-misses on applies and persists the preference")
           (recording-terminal-reset terminal)
           (application-cache-miss-notices-command application)
           (test-assert
            (search "notices are on" (recording-terminal-output terminal))
            "/cache-misses without an argument reports the current state")
           (application-cache-miss-notices-command application nil)
           (test-assert
            (and (not (application-cache-miss-notices-p application))
                 (not (config :cache-miss-notices-p configuration)))
            "/cache-misses off applies and persists the preference")
           (test-assert
            (handler-case
                (progn
                  (application-cache-miss-notices-command application "loud")
                  nil)
              (configuration-error ()
                t))
            "an unknown /cache-misses argument is rejected")
           (test-assert
            (search "/cache-misses [on|off]" (application-help))
            "help lists the cache-miss notice command"))
      (ignore-errors (terminal-ui-stop ui))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
  nil)
