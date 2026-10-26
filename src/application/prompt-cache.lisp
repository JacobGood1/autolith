(in-package #:autolith)

;;;; -- Prompt-Cache Misses --

(defparameter *prompt-cache-lifetime-seconds* 300
  "The inactivity heuristic used to explain cache misses, not a provider expiry guarantee.")

(defparameter *prompt-cache-miss-minimum-tokens* 1024
  "The smallest previous prompt used to flag low cache reads.")

(defparameter *prompt-cache-miss-ratio* 1/2
  "Without a previous cache count, reads below this share of the previous prompt count as a miss.")

(defparameter *prompt-cache-stall-minimum-tokens* 4096
  "The smallest unread growth behind a non-advancing cache count used to flag stagnation.")

(defparameter *prompt-cache-stall-ratio* 1/16
  "The previous-prompt share unread growth must reach to flag stagnation.")

(defparameter *prompt-cache-block-tokens* 128
  "The provider's cache granularity: the smallest drop in cache reads that counts as lost prefix.")

(deftype prompt-cache-anomaly-kind ()
  "How cache reads compare with the previous request's.

:SHRUNK means fewer tokens were read than before, so a cached prefix was lost.
:STALLED means reads did not advance although the prompt grew by enough to cache.
:MISSED means reads were low against a previous request whose own reads are unknown."
  '(member :shrunk :stalled :missed))

(deftype prompt-cache-miss-cause ()
  "A local observation accompanying low cache reads, or an unknown cause."
  '(member :model-changed :idle :prompt-shrank :resumed :unknown))

(defclass prompt-cache-episode ()
  ((conversation
    :initarg       :conversation
    :reader        prompt-cache-episode-conversation
    :type          conversation
    :documentation "The exact conversation served by this request.")
   (provider
    :initarg       :provider
    :reader        prompt-cache-episode-provider
    :type          model-provider
    :documentation "The provider instance whose cache may contain the prompt.")
   (model
    :initarg       :model
    :reader        prompt-cache-episode-model
    :type          string
    :documentation "The model selected when the request started.")
   (process-id
    :initform      (current-process-id)
    :reader        prompt-cache-episode-process-id
    :type          integer
    :documentation "The process that established the local monotonic cache clock.")
   (started-at
    :initarg       :started-at
    :reader        prompt-cache-episode-started-at
    :type          real
    :documentation "The monotonic request start in seconds, before response generation.")
   (prompt-tokens
    :initform      nil
    :accessor      prompt-cache-episode-prompt-tokens
    :type          (option (integer 0))
    :documentation "The completed request's prompt size, or NIL while in flight.")
   (history-characters
    :initform      0
    :accessor      prompt-cache-episode-history-characters
    :type          (integer 0)
    :documentation "The bounded textual history size eligible for summarization.")
   (sequence
    :initform      nil
    :accessor      prompt-cache-episode-sequence
    :type          (option (integer 0))
    :documentation "The durable conversation boundary after request completion."))
  (:documentation "One process-local cache opportunity, consumed before idle compaction."))

(defclass prompt-cache-baseline ()
  ((prompt-tokens
    :initarg       :prompt-tokens
    :reader        prompt-cache-baseline-prompt-tokens
    :type          (integer 0)
    :documentation "The previous request's full prompt size, including temporary context.")
   (cached-tokens
    :initarg       :cached-tokens
    :initform      nil
    :reader        prompt-cache-baseline-cached-tokens
    :type          (option (integer 0))
    :documentation "How much of that prompt the cache already served, when reported.")
   (completed-at
    :initarg  :completed-at
    :initform nil
    :reader   prompt-cache-baseline-completed-at
    :type     (option timestamp)
    :documentation
    "When that request completed, or NIL when it belongs to a resumed conversation.")
   (model
    :initarg       :model
    :initform      nil
    :reader        prompt-cache-baseline-model
    :type          (option string)
    :documentation "The model that served that request, or NIL when unknown."))
  (:documentation "Previous request measurements, not proof of a reusable prefix."))

(-> prompt-cache-baseline-create
    (t &key (:completed-at (option timestamp)) (:model (option string)))
    (option prompt-cache-baseline))
(defun prompt-cache-baseline-create (usage &key completed-at model)
  "Return previous request measurements from USAGE, or NIL without a prompt size."
  (let ((prompt-tokens (conversation--usage-field usage "input_tokens")))
    (and prompt-tokens
         (make-instance 'prompt-cache-baseline
                        :prompt-tokens prompt-tokens
                        :cached-tokens (conversation--usage-field
                                        usage "cached_input_tokens")
                        :completed-at  completed-at
                        :model         model))))

(-> prompt-cache-baseline-advance
    ((option prompt-cache-baseline) t
     &key (:completed-at (option timestamp)) (:model (option string)) (:reported-p boolean))
    (option prompt-cache-baseline))
(defun prompt-cache-baseline-advance (baseline usage &key completed-at model reported-p)
  "Return the baseline following BASELINE after a request completing with USAGE.

A loss of cache reads too small to report keeps BASELINE's reads as the
reference, so repeated small losses accumulate until one report covers them.
When REPORTED-P or the reads did not fall, USAGE's own reads become it."
  (let ((next (prompt-cache-baseline-create usage :completed-at completed-at :model model)))
    (if (and next baseline (not reported-p)
             (prompt-cache-baseline-cached-tokens baseline)
             (prompt-cache-baseline-cached-tokens next)
             (< (prompt-cache-baseline-cached-tokens next)
                (prompt-cache-baseline-cached-tokens baseline)))
        (make-instance 'prompt-cache-baseline
                       :prompt-tokens (prompt-cache-baseline-prompt-tokens next)
                       :cached-tokens (prompt-cache-baseline-cached-tokens baseline)
                       :completed-at  completed-at
                       :model         model)
        next)))

(-> prompt-cache-baseline-from-conversation
    (conversation)
    (option prompt-cache-baseline))
(defun prompt-cache-baseline-from-conversation (conversation)
  "Return the baseline from CONVERSATION's checkpointed provider usage.

Persisted usage carries neither time nor model. A report against this baseline
can identify a resumed session, but cannot establish why cache reads were low."
  (prompt-cache-baseline-create (conversation-prompt-cache-usage conversation)))

(-> prompt-cache--miss-cause
    (&key (:model-changed-p boolean)
          (:idle-seconds (option (integer 0)))
          (:shrunk-p boolean)
          (:completed-at (option timestamp)))
    prompt-cache-miss-cause)
(defun prompt-cache--miss-cause
    (&key model-changed-p idle-seconds shrunk-p completed-at)
  "Return a known local observation, never infer a prefix change from token counts."
  (cond
    (model-changed-p
     ':model-changed)
    ((and idle-seconds (> idle-seconds *prompt-cache-lifetime-seconds*))
     ':idle)
    (shrunk-p
     ':prompt-shrank)
    ((null completed-at)
     ':resumed)
    (t
     ':unknown)))

(-> prompt-cache--usage-entry (t string) t)
(defun prompt-cache--usage-entry (usage name)
  "Return the nested value NAME carries in portable or wire USAGE data."
  (cond
    ((json-object-p usage)
     (json-get usage name))
    ((listp usage)
     (second (assoc name usage :test #'equal)))
    (t
     nil)))

(-> prompt-cache--attribution-uncached (t string) (option (integer 0)))
(defun prompt-cache--attribution-uncached (usage field)
  "Return measured uncached tokens for request FIELD from server attribution.

Attribution separates instruction and tool usage when the provider reports it.
It does not establish why those tokens were uncached. NIL means no attribution."
  (let* ((attribution (prompt-cache--usage-entry usage "attribution"))
         (fields      (and attribution
                           (prompt-cache--usage-entry attribution "request_fields")))
         (entry       (and fields (prompt-cache--usage-entry fields field)))
         (input       (and entry (prompt-cache--usage-entry entry "input_tokens")))
         (cached      (and entry (prompt-cache--usage-entry entry "cached_tokens"))))
    (when (and (integerp input) (integerp cached))
      (max 0 (- input cached)))))

(-> prompt-cache--anomaly
    (prompt-cache-baseline (integer 0) (integer 0))
    (values (option prompt-cache-anomaly-kind) (integer 0)))
(defun prompt-cache--anomaly (baseline prompt-tokens cached-tokens)
  "Return the anomaly kind of reading CACHED-TOKENS of PROMPT-TOKENS after BASELINE, and its lost tokens.

Reads are compared with the previous request's reads, the comparable cached
prefix, not with its whole prompt: a read that grew is progress however far it
trails the prompt. Shrinking reads lose exactly the difference. Reads that do
not advance lose the previous prompt's unread part, which accumulates over
consecutive stalled requests; it includes that request's temporary context, so
it is an upper bound. Without previous reads, low reads against the previous
prompt are the only evidence. The first value is NIL when nothing is anomalous."
  (let ((previous        (prompt-cache-baseline-prompt-tokens baseline))
        (previous-cached (prompt-cache-baseline-cached-tokens baseline)))
    (cond
      ((null previous-cached)
       (if (and (>= previous *prompt-cache-miss-minimum-tokens*)
                (< cached-tokens (* previous *prompt-cache-miss-ratio*)))
           (values ':missed (max 0 (- previous cached-tokens)))
           (values nil 0)))
      ((>= (- previous-cached cached-tokens) *prompt-cache-block-tokens*)
       (values ':shrunk (- previous-cached cached-tokens)))
      ((and (> prompt-tokens previous)
            (<= cached-tokens previous-cached)
            (>= (- previous previous-cached)
                (max *prompt-cache-stall-minimum-tokens*
                     (floor (* previous *prompt-cache-stall-ratio*)))))
       (values ':stalled (max 0 (- previous cached-tokens))))
      (t
       (values nil 0)))))

(-> prompt-cache-miss-detect
    ((option prompt-cache-baseline) t
     &key (:started-at (option timestamp)) (:model (option string)))
    (option list))
(defun prompt-cache-miss-detect (baseline usage &key started-at model)
  "Return measurements when USAGE's cache reads fall behind the previous request's.

USAGE is the completed request's usage; STARTED-AT and MODEL describe that
request. :KIND is a PROMPT-CACHE-ANOMALY-KIND and :LOST-TOKENS the reuse it
missed, as PROMPT-CACHE--ANOMALY measures them; the current request's new
suffix is never counted as lost. :CAUSE identifies a local observation or
:UNKNOWN, not a proven cache cause. Requests without counters, or whose reads
kept pace, return NIL."
  (block nil
    (unless baseline
      (return nil))
    (let* ((prompt-tokens (conversation--usage-field usage "input_tokens"))
           (cached-tokens (conversation--usage-field usage "cached_input_tokens"))
           (expected      (prompt-cache-baseline-prompt-tokens baseline)))
      (unless (and prompt-tokens cached-tokens)
        (return nil))
      (multiple-value-bind (kind lost-tokens)
          (prompt-cache--anomaly baseline prompt-tokens cached-tokens)
        (unless kind
          (return nil))
        (let* ((completed-at   (prompt-cache-baseline-completed-at baseline))
               (baseline-model (prompt-cache-baseline-model baseline))
               (idle-seconds
                 (and completed-at
                      started-at
                      (max 0 (- started-at completed-at)))))
          (list :kind                   kind
                :lost-tokens            lost-tokens
                :prompt-tokens          prompt-tokens
                :cached-tokens          cached-tokens
                :previous-cached-tokens (prompt-cache-baseline-cached-tokens baseline)
                :cause                  (prompt-cache--miss-cause
                                         :model-changed-p (and model
                                                               baseline-model
                                                               (string/= model baseline-model)
                                                               t)
                                         :idle-seconds    idle-seconds
                                         :shrunk-p        (< prompt-tokens expected)
                                         :completed-at    completed-at)
                :idle-seconds           idle-seconds
                :instructions-uncached
                (prompt-cache--attribution-uncached usage "instructions")
                :tools-uncached
                (prompt-cache--attribution-uncached usage "tools")))))))
