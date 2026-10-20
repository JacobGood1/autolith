(in-package #:autolith)

;;;; -- Structured Knowledge Tests --

(-> knowledge-tests--error-code (function) (option keyword))
(defun knowledge-tests--error-code (function)
  "Return the KNOWLEDGE-ERROR code FUNCTION signals, or NIL when it returns."
  (handler-case
      (progn
        (funcall function)
        nil)
    (knowledge-error (condition)
      (knowledge-error-code condition))))


(-> test-knowledge-terms () null)
(defun test-knowledge-terms ()
  "Test term parsing, canonical forms, comparison, and presentation."
  (dolist (case '(("2026-10-22T13:00:00Z" :instant (:instant "2026-10-22T13:00:00Z"))
                  ("2026-10-22T15:00+02:00" :instant (:instant "2026-10-22T13:00:00Z"))
                  ("2026-10-22T07:30:15.250-0530" :instant (:instant "2026-10-22T13:00:15Z"))
                  ("2026-10-22" :date (:date "2026-10-22"))
                  ("PT30M" :duration (:duration 1800))
                  ("P1DT2H" :duration (:duration 93600))
                  ("-PT5M" :duration (:duration -300))
                  (90 :duration (:duration 90))
                  ("2.5" :number 5/2)
                  (7 :number 7)
                  ("true" :boolean :true)
                  ("event-42" :entity "event-42")
                  (("30" "km") :quantity :invalid)
                  ((30 "km") :quantity (:quantity 30 "km"))
                  ((12 "EUR") :quantity (:quantity 12 "EUR"))))
    (destructuring-bind (input type expected) case
      (test-assert (if (eq expected :invalid)
                       (eq (knowledge-tests--error-code
                            (lambda () (knowledge-term-from-input input type)))
                           ':invalid-term)
                       (equal (knowledge-term-from-input input type) expected))
                   (format nil "~S as ~A reads as ~S" input type expected))))
  (dolist (case '(("2026-10-22T13:00" :instant)
                  ("2026-02-30" :date)
                  ("P1M" :duration)
                  ("PT" :duration)
                  ("" :entity)
                  ((3 "furlongs") :quantity)
                  ("maybe" :boolean)))
    (destructuring-bind (input type) case
      (test-assert (eq (knowledge-tests--error-code
                        (lambda () (knowledge-term-from-input input type)))
                       ':invalid-term)
                   (format nil "~S is rejected as ~A" input type))))
  (test-assert (and (= (knowledge-term-compare '(:quantity 1 "km") '(:quantity 999 "m")) 1)
                    (knowledge-term= '(:quantity 1 "km") '(:quantity 1000 "m"))
                    (= (knowledge-term-compare '(:instant "2026-10-22T13:00:00Z")
                                               '(:instant "2026-10-22T14:00:00Z"))
                       -1)
                    (= (knowledge-term-compare '(:duration 60) '(:duration 60)) 0))
               "terms compare within their class, converting units")
  (test-assert (eq (knowledge-tests--error-code
                    (lambda ()
                      (knowledge-term-compare '(:quantity 1 "km") '(:quantity 1 "kg"))))
                   ':incomparable)
               "different dimensions do not compare")
  (test-assert (not (knowledge-term= '(:quantity 1 "EUR") '(:quantity 1 "USD")))
               "different currencies are different values")
  (test-assert (equal (mapcar #'knowledge-term-display
                              '((:instant "2026-10-22T13:00:00Z") (:duration 5430)
                                (:quantity 5/2 "km") 1/3 :false))
                      '("2026-10-22 13:00 UTC" "1h 30m 30s" "2.5 km" "0.333333" "false"))
               "terms display compactly for people")
  (test-assert (and (knowledge-term-p '(:instant "2026-10-22T13:00:00Z"))
                    (not (knowledge-term-p '(:instant "2026-10-22T15:00:00+02:00")))
                    (not (knowledge-term-p '(:duration 1.5)))
                    (not (knowledge-term-p '(:quantity 3 "furlongs"))))
               "only canonical forms are terms")
  nil)

(-> knowledge-tests--event-schema () list)
(defun knowledge-tests--event-schema ()
  "Return a schema keyed by its event, so one event has one start."
  (knowledge-make-schema :predicate "event.start"
                         :arguments '((:name "event" :type :entity)
                                      (:name "time" :type :instant))
                         :key '("event")
                         :template "{event} starts at {time}"))


(-> test-knowledge-schemas () null)
(defun test-knowledge-schemas ()
  "Test schemas, canonical propositions, claim identity, and conflict keys."
  (let* ((schema (knowledge-tests--event-schema))
         (start (knowledge-make-proposition
                 schema '("review" "2026-10-22T15:00+02:00")))
         (same (knowledge-make-proposition
                schema '("review" "2026-10-22T13:00:00Z")))
         (later (knowledge-make-proposition
                 schema '("review" "2026-10-22T14:00:00Z")))
         (denied (knowledge-make-proposition
                  schema '("review" "2026-10-22T13:00:00Z") :negated t)))
    (test-assert (equal start '("event.start" "review" (:instant "2026-10-22T13:00:00Z")))
                 "propositions are canonical whatever the input zone")
    (test-assert (and (string= (knowledge-claim-identifier start :scope ':workspace
                                                                  :workspace "/w/")
                               (knowledge-claim-identifier same :scope ':workspace
                                                                :workspace "/w/"))
                      (string/= (knowledge-claim-identifier start :scope ':workspace
                                                                  :workspace "/w/")
                                (knowledge-claim-identifier start :scope ':global))
                      (string/= (knowledge-claim-identifier start :scope ':global)
                                (knowledge-claim-identifier denied :scope ':global)))
                 "one proposition in one scope is one claim")
    (test-assert (and (equal (knowledge-proposition-key start schema)
                             (knowledge-proposition-key later schema))
                      (not (knowledge-propositions-equal-p start later))
                      (null (knowledge-proposition-key denied schema)))
                 "claims keyed by the same event with other times conflict")
    (test-assert (equal (list (knowledge-proposition-display start schema)
                              (knowledge-proposition-display denied nil))
                        '("review starts at 2026-10-22 13:00 UTC"
                          "not: event.start(review, 2026-10-22 13:00 UTC)"))
                 "propositions read as sentences through their template")
    (dolist (case (list (list "a wrong arity"
                              (lambda () (knowledge-make-proposition schema '("review"))))
                        (list "a rule variable"
                              (lambda () (knowledge-make-proposition
                                          schema '("?event" "2026-10-22T13:00:00Z"))))))
      (test-assert (eq (knowledge-tests--error-code (second case)) ':invalid-claim)
                   (format nil "~A is not a claim" (first case))))
    (test-assert (handler-case (progn (knowledge-make-proposition schema '("review" "1pm")) nil)
                   (knowledge-error (condition)
                     (and (eq (knowledge-error-code condition) ':invalid-term)
                          (search "Argument time of event.start"
                                  (autolith-error-message condition))
                          t)))
                 "a mistyped argument names the argument it belongs to")
    (dolist (case (list (list "an unqualified predicate"
                              (lambda () (knowledge-make-schema
                                          :predicate "start"
                                          :arguments '((:name "x" :type :entity)))))
                        (list "an unknown type"
                              (lambda () (knowledge-make-schema
                                          :predicate "a.b"
                                          :arguments '((:name "x" :type :colour)))))
                        (list "a duplicate argument"
                              (lambda () (knowledge-make-schema
                                          :predicate "a.b"
                                          :arguments '((:name "x" :type :entity)
                                                       (:name "x" :type :number)))))
                        (list "a key naming no argument"
                              (lambda () (knowledge-make-schema
                                          :predicate "a.b"
                                          :arguments '((:name "x" :type :entity)
                                                       (:name "y" :type :number))
                                          :key '("z"))))
                        (list "a unit on a number"
                              (lambda () (knowledge-make-schema
                                          :predicate "a.b"
                                          :arguments '((:name "x" :type :number :unit "km")))))))
      (test-assert (eq (knowledge-tests--error-code (second case)) ':invalid-schema)
                   (format nil "~A is not a schema" (first case))))
    (let ((distance (knowledge-make-schema
                     :predicate "route.distance"
                     :arguments '((:name "route" :type :entity)
                                  (:name "length" :type :quantity :unit "km")))))
      (test-assert (and (knowledge-make-proposition distance '("r1" (12 "mi")))
                        (eq (knowledge-tests--error-code
                             (lambda ()
                               (knowledge-make-proposition distance '("r1" (12 "kg")))))
                            ':invalid-claim))
                   "a quantity argument accepts only units of its dimension")))
  nil)

(-> knowledge-tests--statement (string) list)
(defun knowledge-tests--statement (quote)
  "Return evidence that the user said QUOTE in a test conversation."
  (knowledge-make-evidence :source (list :user :quote quote :conversation "test")
                           :observed-at 1
                           :adapter "test/1"))

(-> knowledge-tests--reread (configuration) knowledge-base)
(defun knowledge-tests--reread (configuration)
  "Return CONFIGURATION's knowledge base folded again from its log, as after a restart."
  (clrhash *knowledge-base-cache*)
  (knowledge-base-read configuration))


(-> test-knowledge-store () null)
(defun test-knowledge-store ()
  "Test knowledge persistence, support identity, retraction, schemas, and log tolerance."
  (with-test-configuration (configuration)
    (let* ((schema (knowledge-tests--event-schema))
           (thirteen (knowledge-make-proposition schema '("review" "2026-10-22T13:00:00Z"))))
      (test-assert (eq (knowledge-tests--error-code
                        (lambda ()
                          (knowledge-assert configuration thirteen
                                            :evidence (knowledge-tests--statement "it is at one")
                                            :basis ':user-assertion)))
                       ':unknown-predicate)
                   "claims need a defined predicate")
      (knowledge-define-schema configuration schema)
      (test-assert (eq (knowledge-tests--error-code
                        (lambda ()
                          (knowledge-assert configuration thirteen
                                            :scope ':global
                                            :evidence (knowledge-tests--statement "it is at one")
                                            :basis ':user-assertion)))
                       ':unknown-predicate)
                   "a global claim needs a global predicate")
      (multiple-value-bind (claim support)
          (knowledge-assert configuration thirteen
                            :evidence (knowledge-tests--statement "the review is at one")
                            :basis ':user-assertion)
        (let ((revision (knowledge-base-revision (knowledge-base-read configuration))))
          (multiple-value-bind (same-claim same-support)
              (knowledge-assert configuration thirteen
                                :evidence (knowledge-tests--statement "the review is at one")
                                :basis ':user-assertion)
            (test-assert (and (string= claim same-claim)
                              (string= support same-support)
                              (= revision (knowledge-base-revision
                                           (knowledge-base-read configuration))))
                         "asserting the same support twice writes nothing")))
        (multiple-value-bind (second-claim second-support)
            (knowledge-assert configuration thirteen
                              :evidence (knowledge-tests--statement "review moved to one")
                              :basis ':user-assertion)
          (let ((entry (knowledge-base-claim (knowledge-tests--reread configuration) claim)))
            (test-assert (and (string= claim second-claim)
                              (string/= support second-support)
                              (= 2 (length (getf entry :supports))))
                         "independent evidence adds a second support to the same claim after a restart"))
          (test-assert (equal (knowledge-retract configuration second-support
                                                 :reason "misheard")
                              (list second-support))
                       "one support can be retracted")
          (let ((base (knowledge-tests--reread configuration)))
            (test-assert (and (getf (knowledge-base-support base second-support) :retracted-at)
                              (equal (getf (knowledge-base-support base second-support)
                                           :retraction-reason)
                                     "misheard")
                              (null (getf (knowledge-base-support base support) :retracted-at)))
                         "a retracted support stays inspectable beside the active one"))
          (test-assert (equal (knowledge-retract configuration claim) (list support))
                       "retracting a claim retracts its remaining active supports")))
      (multiple-value-bind (assumption-claim assumption-support)
          (knowledge-assert configuration
                            (knowledge-make-proposition schema '("lunch" "2026-10-22T12:00:00Z"))
                            :basis ':assumption)
        (declare (ignore assumption-claim))
        (test-assert (null (getf (knowledge-base-support (knowledge-base-read configuration)
                                                         assumption-support)
                                 :evidence))
                     "an assumption needs no evidence"))
      (test-assert (eq (knowledge-tests--error-code
                        (lambda ()
                          (knowledge-assert configuration thirteen :basis ':observation)))
                       ':invalid-claim)
                   "an observation needs evidence")
      (let ((retemplated (knowledge-make-schema :predicate "event.start"
                                                :arguments '((:name "event" :type :entity)
                                                             (:name "time" :type :instant))
                                                :key '("event")
                                                :template "{event} begins {time}")))
        (test-assert (= 2 (nth-value 1 (knowledge-define-schema configuration retemplated)))
                     "changing a template redefines the schema as version 2")
        (test-assert (eq (knowledge-tests--error-code
                          (lambda ()
                            (knowledge-define-schema
                             configuration
                             (knowledge-make-schema :predicate "event.start"
                                                    :arguments '((:name "event" :type :entity)
                                                                 (:name "day" :type :date))))))
                         ':schema-in-use)
                     "changing arguments of a predicate in use is refused"))
      (let ((base (knowledge-base-read configuration)))
        (test-assert (eq base (knowledge-base-read configuration))
                     "an unchanged log is read once"))
      (let* ((pathname (configuration-knowledge-path configuration))
             (forms (sexp-store:log-read pathname)))
        (sexp-store:log-write pathname (append forms (list '(:future-kind :version 9 :x 1))))
        (with-open-file (stream pathname :direction ':output :if-exists ':append)
          (write-string "(:claim-asserted :version 1 :support" stream))
        (let ((base (knowledge-tests--reread configuration)))
          (test-assert (= (knowledge-base-revision base) (length forms))
                       "unknown record kinds and an interrupted final record are tolerated")))))
  nil)

(-> knowledge-tests--evidence-state (evidence-check list) keyword)
(defun knowledge-tests--evidence-state (check evidence)
  "Return EVIDENCE's applicability under CHECK, from its plist."
  (values (evidence-check-evidence check (rest evidence))))


(-> test-knowledge-applicability () null)
(defun test-knowledge-applicability ()
  "Test when evidence and supports apply: files, expiry, memories, retraction, validity."
  (with-test-configuration (base root)
    (multiple-value-bind (configuration workspace)
        (memory-source-tests--workspace base root)
      (declare (ignore configuration))
      (let* ((schedule (memory-source-tests--write
                        (merge-pathnames "schedule.txt" workspace)
                        (format nil "Review at 13:00 in room 4.~%")))
             (quoted (knowledge-observe-file schedule :quote "Review at 13:00"
                                                      :workspace (namestring workspace)
                                                      :time 1000))
             (whole (knowledge-observe-file schedule :workspace (namestring workspace)
                                                     :time 1000))
             (page (knowledge-observe :source '(:url :url "https://example.com/times")
                                      :adapter "web/1" :time 1000))
             (said (knowledge-observe :source '(:user :quote "the review is at one")
                                      :adapter "user/1" :time 1000))
             (memory (memory-remember base :title "Room" :content "Room 4."))
             (cited (knowledge-observe
                     :source (list :memory :id (memory-identifier memory)
                                   :digest (memory-content-digest "Room" "Room 4."))
                     :adapter "memory/1" :time 1000)))
        (test-assert (and (equal (getf (rest (getf (rest quoted) :source)) :path)
                                 "schedule.txt")
                          (getf (rest (getf (rest quoted) :source)) :relative))
                     "file evidence inside the workspace is recorded relatively")
        (flet ((check (now)
                 (evidence-check-create :now now
                                        :memories (memory-list base :visibility ':all))))
          (let ((check (check 2000)))
            (test-assert (every (lambda (evidence)
                                  (eq (knowledge-tests--evidence-state check evidence)
                                      ':current))
                                (list quoted whole page said cited))
                         "fresh evidence of every kind applies"))
          (test-assert (and (eq (knowledge-tests--evidence-state
                                 (check (+ 1001 *knowledge-url-lifetime*)) page)
                                ':expired)
                            (eq (knowledge-tests--evidence-state
                                 (check (+ 1001 *knowledge-url-lifetime*)) said)
                                ':current))
                       "a web observation expires while a statement does not")
          (memory-source-tests--write schedule (format nil "Review at 13:00 in room 5.~%"))
          (let ((check (check 2000)))
            (test-assert (and (eq (knowledge-tests--evidence-state check quoted) ':current)
                              (eq (knowledge-tests--evidence-state check whole) ':changed))
                         "a quoted passage survives an unrelated edit, a whole-file digest does not"))
          (delete-file schedule)
          (test-assert (eq (knowledge-tests--evidence-state (check 2000) quoted) ':missing)
                       "a deleted file no longer supports anything")
          (memory-remember base :identifier (memory-identifier memory)
                                :title "Room" :content "Room 5.")
          (test-assert (eq (knowledge-tests--evidence-state (check 2000) cited) ':changed)
                       "a replaced memory no longer supports what cited it")
          (let ((store (make-instance 'knowledge-base)))
            (setf (gethash (getf (rest said) :id) (knowledge-base-evidence store))
                  (rest said))
            (dolist (case (list (list (list :evidence (getf (rest said) :id)) ':current)
                                (list (list :evidence (getf (rest said) :id)
                                            :retracted-at 1500)
                                      ':retracted)
                                (list (list :evidence (getf (rest said) :id)
                                            :valid-from 3000)
                                      ':pending)
                                (list (list :evidence (getf (rest said) :id)
                                            :valid-until 1500)
                                      ':expired)
                                (list (list :basis :assumption) ':current)))
              (test-assert (eq (evidence-check-support (check 2000) store (first case))
                               (second case))
                           (format nil "a support ~S is ~A" (first case) (second case)))))))))
  nil)
