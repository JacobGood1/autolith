(in-package #:autolith)

;;;; -- Mechanical Precompaction --

;;; When a provider's prompt cache is cold or its retention estimate has
;;; elapsed, prune context a mechanical rule identifies as irrelevant: tool
;;; exchanges whose result failed, including denied commands and interrupted
;;; calls. Pruning changes only the in-memory provider projection; the durable
;;; log keeps every record, and each cold start prunes again. Failures after
;;; the latest user message stay, because the model is still acting on them.


;;;; -- Public Functions --

(-> conversation-note-failed-call (conversation t) null)
(defun conversation-note-failed-call (conversation call-id)
  "Remember that CALL-ID's tool result failed in CONVERSATION."
  (when (non-empty-string-p call-id)
    (setf (gethash call-id (conversation-failed-call-ids conversation)) t))
  nil)

(-> conversation-precompact (conversation) (integer 0))
(defun conversation-precompact (conversation)
  "Drop failed tool exchanges before the latest user message from CONVERSATION's projection.

A call and its output are dropped together, only when both precede that
message, and a reasoning item goes with them when everything it led to went.
Return how many calls were dropped. The durable log is unchanged."
  (with-recursive-lock-held ((conversation-append-lock conversation))
    (let* ((items (coerce (conversation-input-items conversation) 'list))
           (drop  (conversation-precompaction--droppable conversation items))
           (count (count-if (lambda (item)
                              (and (gethash item drop)
                                   (clinker-transcript:function-call-item-p item)))
                            items)))
      (when (plusp (hash-table-count drop))
        (setf (conversation-input-items conversation)
              (remove-if (lambda (item) (gethash item drop)) items)))
      count)))

(-> conversation-prepare-provider-request
    (conversation &key (:identity list) (:now integer) (:idle-seconds (option real)))
    (integer 0))
(defun conversation-prepare-provider-request
    (conversation &key identity (now (get-universal-time)) idle-seconds)
  "Precompact cold or estimated-expired context, then note this request.

IDENTITY names the provider and model serving the request. Prune on the first
request after resume, a provider/model change, or IDLE-SECONDS since a provider
last processed the prompt, at a request start or a completed response. NIL
means retention is unknown and disables idle pruning. Return how many calls
were dropped."
  (let ((previous-identity  (conversation-request-identity conversation))
        (previous-processed (conversation-prompt-processed-at conversation)))
    (prog1 (if (if previous-identity
                   (or (not (equal identity previous-identity))
                       (and previous-processed idle-seconds (plusp idle-seconds)
                            (>= (- now previous-processed) idle-seconds)))
                   (conversation-resumed-p conversation))
               (conversation-precompact conversation)
               0)
      (setf (conversation-request-identity conversation)    identity
            (conversation-prompt-processed-at conversation) now))))

(-> conversation-note-provider-response (conversation &key (:now integer)) null)
(defun conversation-note-provider-response (conversation &key (now (get-universal-time)))
  "Note that a provider just completed a response to CONVERSATION's prompt.

The prompt cache is warm from that moment, however long the request spent
failing and retrying before it, so idle pruning measures from here."
  (setf (conversation-prompt-processed-at conversation) now)
  nil)


;;;; -- Private Functions --

(-> conversation-precompaction--droppable (conversation list) hash-table)
(defun conversation-precompaction--droppable (conversation items)
  "Return the set of ITEMS, keyed by identity, that precompaction drops."
  (let* ((failed   (conversation-failed-call-ids conversation))
         (boundary (or (position-if #'conversation-precompaction--user-message-p items
                                    :from-end t)
                       0))
         (head     (subseq items 0 boundary))
         (calls    (make-hash-table :test #'equal))
         (outputs  (make-hash-table :test #'equal))
         (drop     (make-hash-table :test #'eq)))
    (dolist (item head)
      (let ((call-id (and (json-object-p item) (json-get item "call_id"))))
        (when (and call-id (gethash call-id failed))
          (cond
            ((clinker-transcript:function-call-item-p item)
             (setf (gethash call-id calls) item))
            ((clinker-transcript:function-call-output-item-p item)
             (setf (gethash call-id outputs) item))))))
    (maphash (lambda (call-id call)
               (let ((output (gethash call-id outputs)))
                 (when output
                   (setf (gethash call drop)   t
                         (gethash output drop) t))))
             calls)
    (loop for (item . following) on head
          when (and (conversation-precompaction--reasoning-p item)
                    (conversation-precompaction--consequences-dropped-p following drop))
            do (setf (gethash item drop) t))
    drop))

(-> conversation-precompaction--consequences-dropped-p (list hash-table) boolean)
(defun conversation-precompaction--consequences-dropped-p (following drop)
  "Return true when the model output FOLLOWING a reasoning item exists and is all in DROP.

The output runs until the next user message or tool output, the next input."
  (let ((consequences
          (loop for item in following
                until (or (conversation-precompaction--user-message-p item)
                          (clinker-transcript:function-call-output-item-p item))
                unless (conversation-precompaction--reasoning-p item)
                  collect item)))
    (and consequences
         (every (lambda (item) (gethash item drop)) consequences)
         t)))

(-> conversation-precompaction--user-message-p (t) boolean)
(defun conversation-precompaction--user-message-p (item)
  "Return true when ITEM is a user message."
  (and (json-object-p item)
       (equal (json-get item "role") "user")
       t))

(-> conversation-precompaction--reasoning-p (t) boolean)
(defun conversation-precompaction--reasoning-p (item)
  "Return true when ITEM is reasoning recorded with a model's output."
  (and (json-object-p item)
       (or (clinker-transcript:reasoning-item-p item)
           (clinker-transcript:chat-reasoning-item-p item))
       t))
