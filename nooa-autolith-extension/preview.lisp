(in-package #:autolith)

;;;; -- Bounded Live-Value Previews --

(defparameter *value-preview-head-count* 4
  "The number of leading sequence elements shown in a live-value preview.")

(defparameter *value-preview-tail-count* 4
  "The number of trailing sequence elements shown in a live-value preview.")

(defparameter *value-preview-string-head* 24
  "The number of leading string characters shown in a live-value preview.")

(defparameter *value-preview-string-tail* 24
  "The number of trailing string characters shown in a live-value preview.")

(defparameter *value-preview-depth* 2
  "The maximum nested structure depth rendered in a live-value preview.")

(defparameter *value-preview-character-limit* 512
  "The maximum character count of one live-value preview.")

(defparameter *value-preview-hash-sample* 4
  "The number of hash-table keys sampled in a live-value preview.")


(-> value-preview-internal-slot-name-p (symbol) boolean)
(defun value-preview-internal-slot-name-p (name)
  "Return true when slot NAME is an implementation slot hidden from previews."
  (let ((string (symbol-name name)))
    (and (>= (length string) 2)
         (char= (char string 0) #\-)
         (char= (char string 1) #\-)
         t)))

(-> value-preview--truncate (string) string)
(defun value-preview--truncate (text)
  "Return TEXT clipped to *VALUE-PREVIEW-CHARACTER-LIMIT*."
  (let ((limit *value-preview-character-limit*))
    (if (<= (length text) limit)
        text
        (format nil "~A ... [~D characters omitted]"
                (subseq text 0 limit)
                (- (length text) limit)))))

(-> value-preview--write-atom (t) string)
(defun value-preview--write-atom (value)
  "Return a compact unreadable rendering of atomic VALUE."
  (with-standard-io-syntax
    (let ((*print-pretty* nil)
          (*print-circle* t)
          (*print-length* 16)
          (*print-level* 4)
          (*print-readably* nil))
      (prin1-to-string value))))

(-> value-preview--sequence-tail (t (integer 0) (integer 0)) t)
(defun value-preview--sequence-tail (sequence length tail)
  "Return the last TAIL elements of SEQUENCE of LENGTH, or NIL."
  (cond
    ((zerop tail)
     nil)
    ((listp sequence)
     (nthcdr (max 0 (- length tail)) sequence))
    (t
     (let ((start (max 0 (- length tail)))
           (parts '()))
       (loop for index from start below length
             do (push (aref sequence index) parts))
       (nreverse parts)))))

(-> value-preview--render
    (t hash-table (integer 0))
    string)
(defun value-preview--render (value seen depth)
  "Return the preview of VALUE while tracking circularity in SEEN."
  (when (minusp depth)
    (return-from value-preview--render "#"))
  (cond
    ((null value)
     "NIL")
    ((eq value t)
     "T")
    ((eq value false)
     "FALSE")
    ((eq value :null)
     "NULL")
    ((or (numberp value) (characterp value) (symbolp value) (pathnamep value))
     (value-preview--write-atom value))
    ((stringp value)
     (let ((length (length value))
           (head *value-preview-string-head*)
           (tail *value-preview-string-tail*))
       (if (<= length (+ head tail))
           (format nil "STRING ~D ~S" length value)
           (format nil "STRING ~D ~S ... ~S"
                   length
                   (subseq value 0 head)
                   (subseq value (- length tail))))))
    ((gethash value seen)
     "#CIRCULAR")
    ((consp value)
     (setf (gethash value seen) t)
     (let* ((length (handler-case
                        (list-length value)
                      (error ()
                        nil)))
            (head *value-preview-head-count*)
            (tail *value-preview-tail-count*))
       (cond
         ((null length)
          "IMPROPER-LIST")
         ((<= length (+ head tail))
          (format nil "LIST ~D (~{~A~^ ~})"
                  length
                  (mapcar (lambda (element)
                            (value-preview--render element seen (1- depth)))
                          value)))
         (t
          (format nil "LIST ~D (~{~A~^ ~}) ... (~{~A~^ ~})"
                  length
                  (mapcar (lambda (element)
                            (value-preview--render element seen (1- depth)))
                          (subseq value 0 head))
                  (mapcar (lambda (element)
                            (value-preview--render element seen (1- depth)))
                          (value-preview--sequence-tail
                           value length tail)))))))
    ((and (vectorp value) (not (stringp value)))
     (setf (gethash value seen) t)
     (let ((length (length value))
           (head *value-preview-head-count*)
           (tail *value-preview-tail-count*))
       (flet ((element (index)
                (value-preview--render (aref value index) seen (1- depth))))
         (if (<= length (+ head tail))
             (format nil "VECTOR ~D (~{~A~^ ~})"
                     length
                     (loop for index from 0 below length
                           collect (element index)))
             (format nil "VECTOR ~D (~{~A~^ ~}) ... (~{~A~^ ~})"
                     length
                     (loop for index from 0 below head
                           collect (element index))
                     (loop for index from (max 0 (- length tail)) below length
                           collect (element index)))))))
    ((hash-table-p value)
     (setf (gethash value seen) t)
     (let ((count (hash-table-count value))
           (sample '())
           (limit *value-preview-hash-sample*))
       (maphash
        (lambda (key child)
          (when (< (length sample) limit)
            (push (format nil "~A=~A"
                          (value-preview--render key seen (1- depth))
                          (value-preview--render child seen (1- depth)))
                  sample)))
        value)
       (format nil "HASH-TABLE count=~D sample=(~{~A~^ ~})"
               count (nreverse sample))))
    ((typep value 'standard-object)
     (setf (gethash value seen) t)
     (closer-mop:finalize-inheritance (class-of value))
     (let ((slots '()))
       (dolist (slot (class-slots (class-of value)))
         (let ((name (slot-definition-name slot)))
           (when (and (not (value-preview-internal-slot-name-p name))
                      (slot-boundp value name))
             (push (format nil "~A=~A"
                           (symbol-name name)
                           (value-preview--render
                            (slot-value value name) seen (1- depth)))
                   slots))))
       (format nil "#<~A~@[ ~{~A~^ ~}~]>"
               (class-name (class-of value))
               (nreverse slots))))
    (t
     (format nil "#<~A>" (type-of value)))))

(-> value-preview
    (t &key (:head (integer 0))
            (:tail (integer 0))
            (:depth (integer 0))
            (:limit (integer 1)))
    string)
(defun value-preview
    (value &key (head *value-preview-head-count*)
                (tail *value-preview-tail-count*)
                (depth *value-preview-depth*)
                (limit *value-preview-character-limit*))
  "Return a bounded preview of live VALUE.

Sequences and strings show length plus head and tail samples. Standard
objects show their class and bound public slots. Internal slots whose
names start with a double hyphen are omitted. Circular structure is
rendered as #CIRCULAR instead of looping."
  (let ((*value-preview-head-count* head)
        (*value-preview-tail-count* tail)
        (*value-preview-depth* depth)
        (*value-preview-character-limit* limit))
    (value-preview--truncate
     (value-preview--render value (make-hash-table :test #'eq) depth))))
