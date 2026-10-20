(in-package #:autolith)

;;;; -- Sourced Memory Tests --

(-> memory-source-tests--write (pathname string) pathname)
(defun memory-source-tests--write (pathname content)
  "Replace PATHNAME with exact UTF-8 CONTENT and return PATHNAME."
  (ensure-directories-exist pathname)
  (with-open-file (stream pathname
                          :direction ':output
                          :if-exists ':supersede
                          :if-does-not-exist ':create
                          :external-format ':utf-8)
    (write-string content stream))
  pathname)

(-> memory-source-tests--workspace (configuration pathname)
    (values configuration pathname))
(defun memory-source-tests--workspace (configuration root)
  "Return CONFIGURATION working in a fresh workspace below ROOT, and that workspace."
  (let ((workspace (merge-pathnames "workspace/" root)))
    (ensure-directories-exist workspace)
    (values (memory-tests--configuration-in-workspace configuration workspace)
            workspace)))

(-> memory-source-tests--error-p (function) boolean)
(defun memory-source-tests--error-p (function)
  "Return true when FUNCTION signals MEMORY-ERROR."
  (handler-case
      (progn
        (funcall function)
        nil)
    (memory-error ()
      t)))


(-> test-memory-source-records () null)
(defun test-memory-source-records ()
  "Test durable sources: observation, placement, replay, replacement, and validation."
  (with-test-configuration (base root)
    (multiple-value-bind (configuration workspace)
        (memory-source-tests--workspace base root)
      (let* ((notes (memory-source-tests--write
                     (merge-pathnames "docs/notes.txt" workspace)
                     (format nil "Deploy with~%    make release-prod~%after tagging.")))
             (observed (memory-source-observe-file
                        notes
                        :quote "with make   release-prod"
                        :time (get-universal-time)))
             (sourced (memory-remember configuration
                                       :title "Deploy command"
                                       :content "Deploy with make release-prod."
                                       :sources (list observed)))
             (plain (memory-remember configuration
                                     :title "Plain"
                                     :content "No evidence attached.")))
        (test-assert (equal (getf (rest observed) :path)
                            (uiop:native-namestring notes))
                     "an observed file source starts absolute")
        (let ((source (first (memory-sources sourced))))
          (test-assert (and (equal (getf (rest source) :path) "docs/notes.txt")
                            (eq (getf (rest source) :relative) t))
                       "a workspace memory records files inside it relatively")
          (test-assert (equal (memory-sources
                               (memory-find configuration (memory-identifier sourced)))
                              (memory-sources sourced))
                       "sources survive replay from the memory log")
          (test-assert (equal (memory-source-file-pathname
                               source (memory-workspace sourced))
                              notes)
                       "a relative source resolves against its memory's workspace"))
        (test-assert (null (nth-value 2 (get-properties (rest (memory--record plain))
                                                        '(:sources :premises))))
                     "memories without evidence keep their earlier record form")
        (let ((kept (memory-remember configuration
                                     :identifier (memory-identifier sourced)
                                     :title "Deploy command"
                                     :content "Deploy with make release-prod after tagging.")))
          (test-assert (equal (memory-sources kept) (memory-sources sourced))
                       "a replacement that omits sources keeps them"))
        (let ((global (memory-remember configuration
                                       :identifier (memory-identifier sourced)
                                       :scope ':global
                                       :title "Deploy command"
                                       :content "Deploy with make release-prod.")))
          (test-assert (let ((source (first (memory-sources global))))
                         (and (equal (getf (rest source) :path)
                                      (uiop:native-namestring notes))
                              (null (getf (rest source) :relative))))
                       "moving a memory to global scope records its files absolutely"))
        (let ((dropped (memory-remember configuration
                                        :identifier (memory-identifier sourced)
                                        :title "Deploy command"
                                        :content "Deploy with make release-prod."
                                        :sources nil)))
          (test-assert (null (memory-sources dropped))
                       "an explicit empty source list removes the sources"))
        (test-assert (memory-source-tests--error-p
                      (lambda ()
                        (memory-source-observe-file notes
                                                    :quote "make release-staging"
                                                    :time (get-universal-time))))
                     "a quote absent from the file is rejected")
        (test-assert (memory-source-tests--error-p
                      (lambda ()
                        (memory-source-observe-file notes
                                                    :quote "make"
                                                    :time (get-universal-time))))
                     "a quote too short to identify a passage is rejected")
        (test-assert (memory-source-tests--error-p
                      (lambda ()
                        (memory-source-observe-file
                         (merge-pathnames "missing.txt" workspace)
                         :time (get-universal-time))))
                     "a missing file cannot be cited")
        (dolist (case (list (list "a relative file source in a global memory"
                                  ':global
                                  (list (list :file :path "x.txt" :relative t
                                              :digest (memory-source-text-digest "x")
                                              :observed-at 1)))
                            (list "an unknown source kind"
                                  ':workspace
                                  (list (list :rumor :quote "somebody said so"
                                              :observed-at 1)))
                            (list "a user source without a quote"
                                  ':workspace
                                  (list (list :user :observed-at 1)))))
          (destructuring-bind (label scope sources) case
            (test-assert
             (memory-source-tests--error-p
              (lambda ()
                (memory--record->memory
                 #P"memories.sexp"
                 (list :memory :version *memory-format-version*
                       :id "invalid" :created-at 1 :updated-at 1
                       :scope scope
                       :workspace (and (eq scope ':workspace)
                                       (namestring workspace))
                       :title "Invalid" :content "Invalid sources."
                       :tags nil :source-conversation nil
                       :sources sources))))
             (format nil "~A is rejected when the memory log is read" label)))))))
  nil)

(-> test-memory-premise-records () null)
(defun test-memory-premise-records ()
  "Test derived memories: premise digests, reaffirmation, scope, and cycles."
  (with-test-configuration (base root)
    (multiple-value-bind (configuration workspace)
        (memory-source-tests--workspace base root)
      (declare (ignore workspace))
      (let* ((other (memory-tests--configuration-in-workspace
                     base (merge-pathnames "other/" root)))
             (meeting (memory-remember configuration
                                       :title "Meeting time"
                                       :content "The review meets at 13:00."))
             (travel (memory-remember configuration
                                      :scope ':global
                                      :title "Travel time"
                                      :content "The office is 30 minutes away."))
             (departure (memory-remember configuration
                                         :title "Departure"
                                         :content "Leave at 12:20."
                                         :premises (list (memory-identifier meeting)
                                                         (memory-identifier travel))))
             (foreign (memory-remember other
                                       :title "Elsewhere"
                                       :content "Another workspace fact.")))
        (test-assert (equal (memory-premises departure)
                            (list (list :id (memory-identifier meeting)
                                        :digest (memory-content-digest
                                                 "Meeting time"
                                                 "The review meets at 13:00."))
                                  (list :id (memory-identifier travel)
                                        :digest (memory-content-digest
                                                 "Travel time"
                                                 "The office is 30 minutes away."))))
                     "premises record the exact text they were derived from")
        (memory-remember configuration
                         :identifier (memory-identifier meeting)
                         :title "Meeting time"
                         :content "The review meets at 14:00.")
        (test-assert (equal (getf (first (memory-premises
                                          (memory-find configuration
                                                       (memory-identifier departure))))
                                  :digest)
                            (memory-content-digest "Meeting time"
                                                   "The review meets at 13:00."))
                     "replacing a premise leaves the derived memory's record unchanged")
        (let ((reaffirmed (memory-remember configuration
                                           :identifier (memory-identifier departure)
                                           :title "Departure"
                                           :content "Leave at 13:20.")))
          (test-assert (equal (getf (first (memory-premises reaffirmed)) :digest)
                              (memory-content-digest "Meeting time"
                                                     "The review meets at 14:00."))
                       "replacing a derived memory reaffirms its premises"))
        (dolist (case
                 (list
                  (list "a missing premise"
                        (lambda ()
                          (memory-remember configuration
                                           :title "Orphan" :content "Derived from nothing."
                                           :premises (list "no-such-memory"))))
                  (list "a memory as its own premise"
                        (lambda ()
                          (memory-remember configuration
                                           :identifier (memory-identifier meeting)
                                           :title "Meeting time"
                                           :content "Circular."
                                           :premises (list (memory-identifier meeting)))))
                  (list "a premise cycle"
                        (lambda ()
                          (memory-remember configuration
                                           :identifier (memory-identifier meeting)
                                           :title "Meeting time"
                                           :content "Circular."
                                           :premises (list (memory-identifier departure)))))
                  (list "a global memory relying on a workspace memory"
                        (lambda ()
                          (memory-remember configuration
                                           :scope ':global
                                           :title "Global" :content "Too broad."
                                           :premises (list (memory-identifier meeting)))))
                  (list "a premise from another workspace"
                        (lambda ()
                          (memory-remember configuration
                                           :title "Mixed" :content "Wrong workspace."
                                           :premises (list (memory-identifier foreign)))))))
          (destructuring-bind (label function) case
            (test-assert (memory-source-tests--error-p function)
                         (format nil "~A is rejected" label)))))))
  nil)
