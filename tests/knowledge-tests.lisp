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

(-> knowledge-tests--define-appointments (configuration) null)
(defun knowledge-tests--define-appointments (configuration)
  "Define the appointment vocabulary used by rule and evaluation tests."
  (dolist (definition
           '(("event.start" ((:name "event" :type :entity) (:name "time" :type :instant))
              ("event") "{event} starts at {time}")
             ("event.travel" ((:name "event" :type :entity) (:name "duration" :type :duration))
              ("event") "travel to {event} takes {duration}")
             ("event.buffer" ((:name "event" :type :entity) (:name "duration" :type :duration))
              ("event") "arrive {duration} early for {event}")
             ("event.cancelled" ((:name "event" :type :entity))
              nil "{event} is cancelled")
             ("event.departure" ((:name "event" :type :entity) (:name "time" :type :instant))
              ("event") "leave for {event} at {time}")))
    (destructuring-bind (predicate arguments key template) definition
      (knowledge-define-schema configuration
                               (knowledge-make-schema :predicate predicate
                                                      :arguments arguments
                                                      :key key
                                                      :template template))))
  nil)

(-> knowledge-tests--departure-rule (&key (:id string)) list)
(defun knowledge-tests--departure-rule (&key (id "departure"))
  "Return the departure rule's input fields as keyword arguments."
  (list :id id
        :premises '(("event.start" "?event" "?start")
                    ("event.travel" "?event" "?travel")
                    ("event.buffer" "?event" "?buffer")
                    (:not ("event.cancelled" "?event")))
        :bindings '(("?leave" ("-" "?start" ("+" "?travel" "?buffer"))))
        :conclusion '("event.departure" "?event" "?leave")))

(-> knowledge-tests--schema-function (configuration) function)
(defun knowledge-tests--schema-function (configuration)
  "Return a schema lookup over CONFIGURATION's current knowledge."
  (let ((base (knowledge-base-read configuration)))
    (lambda (predicate)
      (knowledge-base-schema base predicate
                             :workspace (namestring (config :working-directory configuration))))))


(-> test-knowledge-rule-validation () null)
(defun test-knowledge-rule-validation ()
  "Test that rules are range restricted, well typed, bounded, and acyclic."
  (with-test-configuration (configuration)
    (knowledge-tests--define-appointments configuration)
    (let ((schemas (knowledge-tests--schema-function configuration)))
      (test-assert (knowledge-rule-p (apply #'knowledge-make-rule
                                            :schema-function schemas
                                            (knowledge-tests--departure-rule)))
                   "the departure rule is valid")
      (dolist (case
               `(("an unbound conclusion variable"
                  :invalid-rule
                  (:premises (("event.start" "?event" "?start"))
                   :conclusion ("event.departure" "?event" "?leave")))
                 ("a computation on the wrong types"
                  :invalid-rule
                  (:premises (("event.start" "?event" "?start"))
                   :bindings (("?leave" ("-" "?start" 5)))
                   :conclusion ("event.departure" "?event" "?leave")))
                 ("an unknown computation"
                  :invalid-rule
                  (:premises (("event.start" "?event" "?start"))
                   :bindings (("?leave" ("later" "?start")))
                   :conclusion ("event.departure" "?event" "?leave")))
                 ("a negation of an unbound variable"
                  :invalid-rule
                  (:premises (("event.start" "?event" "?start")
                              (:not ("event.cancelled" "?other")))
                   :conclusion ("event.departure" "?event" "?start")))
                 ("a guard across types"
                  :invalid-rule
                  (:premises (("event.start" "?event" "?start")
                              ("event.travel" "?event" "?travel"))
                   :guards (("<" "?start" "?travel"))
                   :conclusion ("event.departure" "?event" "?start")))
                 ("a conclusion of a premise predicate"
                  :rule-cycle
                  (:premises (("event.start" "?event" "?start"))
                   :conclusion ("event.start" "?event" "?start")))
                 ("an undefined predicate"
                  :unknown-predicate
                  (:premises (("event.room" "?event" "?room"))
                   :conclusion ("event.departure" "?event" "?room")))
                 ("a literal of the wrong type"
                  :invalid-term
                  (:premises (("event.start" "review" "one o'clock"))
                   :conclusion ("event.departure" "review" "2026-10-22T12:00:00Z")))))
        (destructuring-bind (label code fields) case
          (test-assert (eq (knowledge-tests--error-code
                            (lambda ()
                              (apply #'knowledge-make-rule :id "r" :schema-function schemas
                                     fields)))
                           code)
                       (format nil "~A is refused as ~(~A~)" label code))))))
  nil)

(-> test-knowledge-rule-computations () null)
(defun test-knowledge-rule-computations ()
  "Test rule computations and guards over typed values."
  (let ((environment (make-hash-table :test #'equal)))
    (setf (gethash "?start" environment) '(:instant "2026-10-22T13:00:00Z")
          (gethash "?travel" environment) '(:duration 1800)
          (gethash "?day" environment) '(:date "2026-10-22")
          (gethash "?far" environment) '(:quantity 2 "km")
          (gethash "?near" environment) '(:quantity 500 "m"))
    (dolist (case '((("-" "?start" ("+" "?travel" (:duration 600))) (:instant "2026-10-22T12:20:00Z"))
                    (("-" "?start" (:instant "2026-10-22T12:00:00Z")) (:duration 3600))
                    (("+" "?day" ("days" 10)) (:date "2026-11-01"))
                    (("+" "?far" "?near") (:quantity 5/2 "km"))
                    (("in-unit" "?far" "m") (:quantity 2000 "m"))
                    (("/" "?travel" (:duration 600)) 3)
                    (("max" "?near" "?far") (:quantity 2 "km"))
                    (("weekday" "?day") 4)
                    (("date" "?start") (:date "2026-10-22"))))
      (destructuring-bind (expression expected) case
        (test-assert (equal (knowledge-evaluate-expression expression environment) expected)
                     (format nil "~S computes ~S" expression expected))))
    (dolist (expression '(("/" "?travel" 0) ("+" "?day" ("hours" 5)) ("+" "?far" (:quantity 1 "kg"))))
      (test-assert (eq (knowledge-tests--error-code
                        (lambda () (knowledge-evaluate-expression expression environment)))
                       ':evaluation)
                   (format nil "~S is an evaluation error" expression)))
    (test-assert (and (knowledge-guard-holds-p '("<" "?near" "?far") environment)
                      (not (knowledge-guard-holds-p '(">=" "?travel" (:duration 3600)) environment))
                      (knowledge-guard-holds-p '("=" "?far" (:quantity 2000 "m")) environment))
                 "guards compare converted values")
    (test-assert (equal (knowledge-expression-from-input #("-" "?start" "PT10M"))
                        '("-" "?start" (:duration 600)))
                 "durations written as text become duration literals"))
  nil)

(-> test-knowledge-rule-lifecycle () null)
(defun test-knowledge-rule-lifecycle ()
  "Test proposing, versioning, admitting, retiring, and cycle refusal of stored rules."
  (with-test-configuration (configuration)
    (knowledge-tests--define-appointments configuration)
    (let ((workspace (namestring (config :working-directory configuration))))
      (multiple-value-bind (id version new-p)
          (apply #'knowledge-propose-rule configuration (knowledge-tests--departure-rule))
        (test-assert (and (string= id "departure") (= version 1) new-p
                          (null (knowledge-base-active-rules (knowledge-base-read configuration)
                                                             :workspace workspace)))
                     "a proposed rule is recorded inactive"))
      (test-assert (null (nth-value 2 (apply #'knowledge-propose-rule configuration
                                             (knowledge-tests--departure-rule))))
                   "proposing the same rule again adds no version")
      (test-assert (= 1 (knowledge-admit-rule configuration "departure" :by "test"))
                   "admitting activates the latest version")
      (apply #'knowledge-propose-rule configuration
             :description "Leave earlier." (knowledge-tests--departure-rule))
      (let ((active (knowledge-base-active-rules (knowledge-tests--reread configuration)
                                                 :workspace workspace)))
        (test-assert (and (= 1 (length active)) (= 1 (second (first active))))
                     "a new proposal leaves the admitted version active until admitted itself"))
      (knowledge-admit-rule configuration "departure")
      (test-assert (= 2 (second (first (knowledge-base-active-rules
                                        (knowledge-base-read configuration)
                                        :workspace workspace))))
                   "admitting the new version replaces the old one")
      (knowledge-define-schema
       configuration
       (knowledge-make-schema :predicate "event.reminder"
                              :arguments '((:name "event" :type :entity)
                                           (:name "time" :type :instant))))
      (knowledge-propose-rule configuration
                              :id "reminder"
                              :premises '(("event.departure" "?event" "?leave"))
                              :bindings '(("?ping" ("-" "?leave" "PT10M")))
                              :conclusion '("event.reminder" "?event" "?ping"))
      (knowledge-admit-rule configuration "reminder")
      (test-assert (eq (knowledge-tests--error-code
                        (lambda ()
                          (knowledge-propose-rule configuration
                                                  :id "loop"
                                                  :premises '(("event.reminder" "?event" "?t"))
                                                  :conclusion '("event.start" "?event" "?t"))))
                       ':rule-cycle)
                   "a rule closing a cycle through admitted rules is refused")
      (knowledge-retire-rule configuration "departure")
      (test-assert (equal (mapcar (lambda (item) (getf (first item) :id))
                                  (knowledge-base-active-rules (knowledge-tests--reread configuration)
                                                               :workspace workspace))
                          '("reminder"))
                   "a retired rule is no longer active after a restart")))
  nil)

(-> knowledge-tests--view (configuration &key (:now timestamp)) knowledge-view)
(defun knowledge-tests--view (configuration &key (now (get-universal-time)))
  "Return CONFIGURATION's knowledge evaluated from its workspace at NOW."
  (knowledge-evaluate (knowledge-base-read configuration)
                      :workspace (namestring (config :working-directory configuration))
                      :check (evidence-check-create
                              :now now
                              :memories (memory-list configuration :visibility ':all))))

(-> knowledge-tests--claim (configuration string list &key (:negated boolean)) list)
(defun knowledge-tests--claim (configuration predicate inputs &key negated)
  "Return the proposition PREDICATE makes of INPUTS under CONFIGURATION's schema."
  (knowledge-make-proposition
   (knowledge-base-schema (knowledge-base-read configuration) predicate
                          :workspace (namestring (config :working-directory configuration)))
   inputs
   :negated negated))

(-> knowledge-tests--said (configuration string list string &key (:negated boolean))
    (values string string))
(defun knowledge-tests--said (configuration predicate inputs quote &key negated)
  "Assert PREDICATE of INPUTS as something the user said in QUOTE."
  (knowledge-assert configuration
                    (knowledge-tests--claim configuration predicate inputs :negated negated)
                    :evidence (knowledge-observe :source (list :user :quote quote)
                                                 :adapter "user/1")
                    :basis ':user-assertion))

(-> knowledge-tests--departures (knowledge-view) list)
(defun knowledge-tests--departures (view)
  "Return the departure times VIEW supports, as canonical instant text."
  (mapcar (lambda (node)
            (second (third (knowledge-node-proposition node))))
          (knowledge-view-holds view '("event.departure" "?event" "?time"))))


(-> test-knowledge-derivation () null)
(defun test-knowledge-derivation ()
  "Test the appointment fixture: a changed source recomputes the departure locally."
  (with-test-configuration (base root)
    (multiple-value-bind (configuration workspace)
        (memory-source-tests--workspace base root)
      (knowledge-tests--define-appointments configuration)
      (knowledge-propose-rule configuration
                              :id "departure"
                              :premises '(("event.start" "?event" "?start")
                                          ("event.travel" "?event" "?travel")
                                          ("event.buffer" "?event" "?buffer"))
                              :bindings '(("?leave" ("-" "?start" ("+" "?travel" "?buffer"))))
                              :conclusion '("event.departure" "?event" "?leave"))
      (knowledge-admit-rule configuration "departure")
      (let ((schedule (memory-source-tests--write
                       (merge-pathnames "schedule.txt" workspace)
                       (format nil "Design review at 13:00 UTC on 22 October.~%"))))
        (knowledge-assert configuration
                          (knowledge-tests--claim configuration "event.start"
                                                  '("review" "2026-10-22T13:00:00Z"))
                          :evidence (knowledge-observe-file schedule
                                                            :quote "review at 13:00"
                                                            :workspace (namestring workspace))
                          :basis ':observation)
        (knowledge-tests--said configuration "event.travel" '("review" "PT30M")
                               "it takes half an hour to get there")
        (knowledge-tests--said configuration "event.buffer" '("review" "PT10M")
                               "be there ten minutes early")
        (let* ((view (knowledge-tests--view configuration))
               (node (first (knowledge-view-holds view '("event.departure" "review" "?t"))))
               (justification (first (knowledge-node-justifications node))))
          (test-assert (equal (knowledge-tests--departures view) '("2026-10-22T12:20:00Z"))
                       "the departure is derived from start, travel, and buffer")
          (test-assert (and (equal (getf justification :rule) "departure")
                            (= (getf justification :version) 1)
                            (= 3 (length (getf justification :premises)))
                            (equal (rest (assoc "?leave" (getf justification :bindings)
                                                :test #'string=))
                                   '(:instant "2026-10-22T12:20:00Z")))
                       "the justification names the rule version, premises, and binding")
          (test-assert (and (equal (knowledge-node-bases node) '((:observation :user-assertion)))
                            (knowledge-node-observed-p node))
                       "the derived claim keeps the bases of its premises"))
        (memory-source-tests--write schedule
                                    (format nil "Design review at 14:00 UTC on 22 October.~%"))
        (let ((view (knowledge-tests--view configuration)))
          (test-assert (null (knowledge-tests--departures view))
                       "a changed schedule withdraws the departure before the new time is known")
          (test-assert (eq (knowledge-node-applicability
                            (knowledge-view-node
                             view
                             (knowledge-claim-identifier
                              (knowledge-tests--claim configuration "event.start"
                                                      '("review" "2026-10-22T13:00:00Z"))
                              :scope ':workspace :workspace (namestring workspace))))
                           ':stale)
                       "the old start is stale because its source changed"))
        (knowledge-assert configuration
                          (knowledge-tests--claim configuration "event.start"
                                                  '("review" "2026-10-22T14:00:00Z"))
                          :evidence (knowledge-observe-file schedule
                                                            :quote "review at 14:00"
                                                            :workspace (namestring workspace))
                          :basis ':observation)
        (test-assert (equal (knowledge-tests--departures (knowledge-tests--view configuration))
                            '("2026-10-22T13:20:00Z"))
                     "recording the new start recomputes the departure without a model")
        (multiple-value-bind (old-buffer)
            (knowledge-tests--said configuration "event.buffer" '("review" "PT10M")
                                   "be there ten minutes early")
          (knowledge-tests--said configuration "event.buffer" '("review" "PT20M")
                                 "make it twenty minutes early")
          (let ((view (knowledge-tests--view configuration)))
            (test-assert (and (= 2 (length (knowledge-view-holds
                                           view '("event.buffer" "review" "?b"))))
                              (every (lambda (node)
                                       (eq (knowledge-node-status view node) ':conflicted))
                                     (knowledge-view-holds view '("event.departure" "review" "?t"))))
                         "two buffers both hold and their departures conflict, with no winner chosen"))
          (knowledge-retract configuration old-buffer :reason "superseded")
          (test-assert (equal (knowledge-tests--departures (knowledge-tests--view configuration))
                              '("2026-10-22T13:10:00Z"))
                       "retracting the old buffer leaves one departure")))))
  nil)

(-> knowledge-tests--admit-departure (configuration &key (:negation boolean)) null)
(defun knowledge-tests--admit-departure (configuration &key negation)
  "Admit the departure rule, requiring an explicit not-cancelled claim when NEGATION."
  (apply #'knowledge-propose-rule configuration
         (if negation
             (knowledge-tests--departure-rule)
             (list :id "departure"
                   :premises '(("event.start" "?event" "?start")
                               ("event.travel" "?event" "?travel")
                               ("event.buffer" "?event" "?buffer"))
                   :bindings '(("?leave" ("-" "?start" ("+" "?travel" "?buffer"))))
                   :conclusion '("event.departure" "?event" "?leave"))))
  (knowledge-admit-rule configuration "departure")
  nil)

(-> knowledge-tests--review (configuration &key (:travel-basis knowledge-basis)) null)
(defun knowledge-tests--review (configuration &key (travel-basis ':user-assertion))
  "Assert the review's start, buffer, and travel, the travel with TRAVEL-BASIS."
  (knowledge-tests--said configuration "event.start" '("review" "2026-10-22T13:00:00Z")
                         "the review starts at one")
  (knowledge-tests--said configuration "event.buffer" '("review" "PT10M")
                         "be there ten minutes early")
  (if (eq travel-basis ':assumption)
      (knowledge-assert configuration
                        (knowledge-tests--claim configuration "event.travel" '("review" "PT30M"))
                        :basis ':assumption)
      (knowledge-tests--said configuration "event.travel" '("review" "PT30M")
                             "half an hour to get there"))
  nil)


(-> test-knowledge-justifications-and-conflicts () null)
(defun test-knowledge-justifications-and-conflicts ()
  "Test alternative support paths, explicit negation, conflicts, and assumption bases."
  (with-test-configuration (configuration)
    (knowledge-tests--define-appointments configuration)
    (knowledge-tests--admit-departure configuration :negation t)
    (knowledge-tests--review configuration)
    (test-assert (null (knowledge-tests--departures (knowledge-tests--view configuration)))
                 "a missing cancellation claim does not satisfy a negated premise")
    (knowledge-tests--said configuration "event.cancelled" '("review")
                           "the review is definitely still on" :negated t)
    (test-assert (equal (knowledge-tests--departures (knowledge-tests--view configuration))
                        '("2026-10-22T12:20:00Z"))
                 "an explicit not-cancelled claim satisfies it")
    (multiple-value-bind (stated)
        (knowledge-tests--said configuration "event.departure" '("review" "2026-10-22T12:20:00Z")
                               "I will leave at twenty past twelve")
      (let ((node (knowledge-view-node (knowledge-tests--view configuration) stated)))
        (test-assert (and (knowledge-node-derived-p node)
                          (find ':current (knowledge-node-supports node)
                                :key (lambda (support) (getf support :state))))
                     "a stated departure the rule also derives has two independent paths"))
      (knowledge-retract configuration stated)
      (test-assert (knowledge-node-supported-p
                    (knowledge-view-node (knowledge-tests--view configuration) stated))
                   "retracting the statement leaves the derivation supporting it"))
    (multiple-value-bind (cancelled)
        (knowledge-tests--said configuration "event.cancelled" '("review")
                               "the review got cancelled after all")
      (let ((view (knowledge-tests--view configuration)))
        (test-assert (eq (knowledge-node-status view (knowledge-view-node view cancelled))
                         ':conflicted)
                     "a claim and its explicit negation both supported conflict")
        (test-assert (equal (knowledge-tests--departures view) '("2026-10-22T12:20:00Z"))
                     "conflicting evidence is shown, not resolved by picking a winner"))
      (knowledge-retract configuration cancelled))
    (knowledge-tests--said configuration "event.departure" '("review" "2026-10-22T12:00:00Z")
                           "I am leaving at noon")
    (let* ((view (knowledge-tests--view configuration))
           (departures (knowledge-view-holds view '("event.departure" "review" "?t"))))
      (test-assert (and (= 2 (length departures))
                        (every (lambda (node) (eq (knowledge-node-status view node) ':conflicted))
                               departures))
                   "two departure times for one event conflict through the schema key")))
  (with-test-configuration (configuration)
    (knowledge-tests--define-appointments configuration)
    (knowledge-tests--admit-departure configuration)
    (knowledge-tests--review configuration :travel-basis ':assumption)
    (let ((node (first (knowledge-view-holds (knowledge-tests--view configuration)
                                             '("event.departure" "review" "?t")))))
      (test-assert (and (equal (knowledge-node-bases node) '((:user-assertion :assumption)))
                        (not (knowledge-node-observed-p node)))
                   "deriving from an assumption does not make an independent observation")))
  nil)

(-> test-knowledge-evaluation-bounds () null)
(defun test-knowledge-evaluation-bounds ()
  "Test expiry, visibility, refused computations, goals, and incomplete evaluation."
  (with-test-configuration (base root)
    (multiple-value-bind (configuration workspace)
        (memory-source-tests--workspace base root)
      (declare (ignore workspace))
      (knowledge-tests--define-appointments configuration)
      (knowledge-tests--admit-departure configuration)
      (knowledge-tests--said configuration "event.start" '("review" "2026-10-22T13:00:00Z")
                             "the review starts at one")
      (knowledge-tests--said configuration "event.buffer" '("review" "PT10M")
                             "be there ten minutes early")
      (knowledge-assert configuration
                        (knowledge-tests--claim configuration "event.travel" '("review" "PT30M"))
                        :evidence (knowledge-observe
                                   :source '(:url :url "https://maps.example.com/route")
                                   :adapter "web/1" :time 1000)
                        :basis ':observation)
      (test-assert (knowledge-tests--departures
                    (knowledge-tests--view configuration :now 2000))
                   "a fresh route estimate supports the departure")
      (let ((view (knowledge-tests--view configuration
                                         :now (+ 1001 *knowledge-url-lifetime*))))
        (test-assert (and (null (knowledge-tests--departures view))
                          (eq (knowledge-node-applicability
                               (knowledge-view-node
                                view
                                (knowledge-claim-identifier
                                 (knowledge-tests--claim configuration "event.travel"
                                                         '("review" "PT30M"))
                                 :scope ':workspace
                                 :workspace (namestring (config :working-directory
                                                                configuration)))))
                              ':expired))
                     "an expired route estimate no longer supports the departure"))
      (let ((other (memory-tests--configuration-in-workspace
                    base (merge-pathnames "other/" root))))
        (ensure-directories-exist (merge-pathnames "other/" root))
        (test-assert (null (knowledge-view-holds (knowledge-tests--view other)
                                                 '("event.start" "?e" "?t")))
                     "another workspace sees none of this workspace's claims"))
      (knowledge-tests--said configuration "event.start" '("standup" "2026-10-22T09:00:00Z")
                             "standup is at nine")
      (knowledge-tests--said configuration "event.buffer" '("standup" "PT0S")
                             "no need to be early for standup")
      (knowledge-tests--said configuration "event.travel" '("standup" "PT5M")
                             "standup is five minutes away")
      (let ((*knowledge-match-limit* 1))
        (let ((view (knowledge-tests--view configuration :now 2000)))
          (test-assert (and (= 1 (length (knowledge-tests--departures view)))
                            (equal (knowledge-view-incomplete view) '("departure")))
                       "a reached work limit marks the rule incomplete instead of denying the rest")))
      (test-assert (null (knowledge-tests--departures
                          (knowledge-evaluate (knowledge-base-read configuration)
                                              :workspace (namestring (config :working-directory
                                                                             configuration))
                                              :check (evidence-check-create :now 2000)
                                              :goals '("event.cancelled"))))
                   "goals skip rules that cannot contribute to them")
      (knowledge-define-schema configuration
                               (knowledge-make-schema
                                :predicate "event.pace"
                                :arguments '((:name "event" :type :entity)
                                             (:name "ratio" :type :number))))
      (knowledge-propose-rule configuration
                              :id "pace"
                              :premises '(("event.travel" "?event" "?travel")
                                          ("event.buffer" "?event" "?buffer"))
                              :bindings '(("?ratio" ("/" "?travel" "?buffer")))
                              :conclusion '("event.pace" "?event" "?ratio"))
      (knowledge-admit-rule configuration "pace")
      (let ((view (knowledge-tests--view configuration :now 2000)))
        (test-assert (and (= 1 (length (knowledge-view-holds view '("event.pace" "?e" "?r"))))
                          (search "Division by zero"
                                  (getf (first (knowledge-view-errors view)) :message)))
                     "a computation refused for one match is reported and the others still derive"))))
  nil)

(-> knowledge-tests--schedule-review (configuration pathname string) pathname)
(defun knowledge-tests--schedule-review (configuration workspace time)
  "Write the schedule with the review at TIME and record the start it states."
  (let ((schedule (memory-source-tests--write
                   (merge-pathnames "schedule.txt" workspace)
                   (format nil "Design review at ~A UTC on 22 October.~%" time))))
    (knowledge-assert configuration
                      (knowledge-tests--claim configuration "event.start"
                                              (list "review"
                                                    (format nil "2026-10-22T~A:00Z" time)))
                      :evidence (knowledge-observe-file
                                 schedule
                                 :quote (format nil "review at ~A" time)
                                 :workspace (namestring workspace))
                      :basis ':observation)
    schedule))


(-> test-knowledge-explanations () null)
(defun test-knowledge-explanations ()
  "Test bounded, paged explanation trees of derived claims."
  (with-test-configuration (base root)
    (multiple-value-bind (configuration workspace)
        (memory-source-tests--workspace base root)
      (knowledge-tests--define-appointments configuration)
      (knowledge-tests--admit-departure configuration)
      (knowledge-tests--schedule-review configuration workspace "13:00")
      (knowledge-tests--said configuration "event.buffer" '("review" "PT10M")
                             "be there ten minutes early")
      (knowledge-tests--said configuration "event.travel" '("review" "PT30M")
                             "half an hour to get there")
      (let* ((view (knowledge-tests--view configuration))
             (departure (knowledge-node-identifier
                         (first (knowledge-view-holds view '("event.departure" "?e" "?t")))))
             (tree (first (knowledge-explain view departure)))
             (justification (first (getf tree :justifications))))
        (test-assert (and (equal (getf tree :text) "leave for review at 2026-10-22 12:20 UTC")
                          (eq (getf tree :status) ':supported)
                          (equal (getf justification :rule) "departure")
                          (= 3 (length (getf justification :premises))))
                     "the explanation names the conclusion, its rule, and its premises")
        (test-assert (find-if (lambda (premise)
                                (let ((support (first (getf premise :supports))))
                                  (and (equal (getf support :text)
                                              "schedule.txt \"review at 13:00\"")
                                       (eq (getf support :state) ':current)
                                       (eq (getf support :basis) ':observation))))
                              (getf justification :premises))
                     "each premise shows the evidence it rests on")
        (multiple-value-bind (trees cursor) (knowledge-explain view departure :limit 2)
          (test-assert (and (= 2 (length cursor))
                            (= 2 (count-if (lambda (premise) (getf premise :more))
                                           (getf (first (getf (first trees) :justifications))
                                                 :premises))))
                       "a page past its limit marks unexpanded premises and returns a cursor")
          (test-assert (= 2 (count-if (lambda (tree) (getf tree :supports))
                                      (knowledge-explain view cursor :limit 2)))
                       "the cursor continues the explanation")))))
  nil)

(-> test-knowledge-changes () null)
(defun test-knowledge-changes ()
  "Test what changed between a snapshot and a later view, with causes."
  (with-test-configuration (base root)
    (multiple-value-bind (configuration workspace)
        (memory-source-tests--workspace base root)
      (knowledge-tests--define-appointments configuration)
      (knowledge-tests--admit-departure configuration)
      (knowledge-tests--schedule-review configuration workspace "13:00")
      (knowledge-tests--said configuration "event.buffer" '("review" "PT10M")
                             "be there ten minutes early")
      (multiple-value-bind (travel)
          (knowledge-tests--said configuration "event.travel" '("review" "PT30M")
                                 "half an hour to get there")
        (let ((snapshot (knowledge-view-snapshot (knowledge-tests--view configuration))))
          (test-assert (null (knowledge-view-changes snapshot (knowledge-tests--view configuration)))
                       "an unchanged view reports no changes")
          (knowledge-tests--schedule-review configuration workspace "14:00")
          (let* ((changes (knowledge-view-changes snapshot (knowledge-tests--view configuration)))
                 (departure (find "leave for review at 2026-10-22 13:20 UTC" changes
                                  :key (lambda (change) (getf change :to)) :test #'equal)))
            (test-assert (and departure
                              (eq (getf departure :kind) ':changed)
                              (equal (getf departure :from)
                                     "leave for review at 2026-10-22 12:20 UTC"))
                         "the departure is reported as moved from its old to its new time")
            (test-assert (find-if (lambda (cause)
                                    (search "schedule.txt \"review at 13:00\" changed" cause))
                                  (getf departure :causes))
                         "the move names the changed file behind it"))
          (let ((moved (knowledge-view-snapshot (knowledge-tests--view configuration))))
            (knowledge-retract configuration travel :reason "taking a taxi")
            (let ((withdrawn (find ':withdrawn (knowledge-view-changes
                                                moved (knowledge-tests--view configuration))
                                   :key (lambda (change) (getf change :kind)))))
              (test-assert (and withdrawn
                                (search "you said \"half an hour to get there\" was retracted"
                                        (first (getf withdrawn :causes))))
                           "a withdrawn departure names the retracted premise"))))))
    nil))

(-> test-knowledge-memory-premises () null)
(defun test-knowledge-memory-premises ()
  "Test prose memories derived from structured claims, stored and derived."
  (with-test-configuration (base root)
    (multiple-value-bind (configuration workspace)
        (memory-source-tests--workspace base root)
      (knowledge-tests--define-appointments configuration)
      (knowledge-tests--admit-departure configuration)
      (knowledge-tests--schedule-review configuration workspace "13:00")
      (knowledge-tests--said configuration "event.buffer" '("review" "PT10M")
                             "be there ten minutes early")
      (knowledge-tests--said configuration "event.travel" '("review" "PT30M")
                             "half an hour to get there")
      (let* ((departure (knowledge-node-identifier
                         (first (knowledge-view-holds (knowledge-tests--view configuration)
                                                      '("event.departure" "?e" "?t")))))
             (note (memory-remember configuration
                                    :title "Review logistics"
                                    :content "Book the 12:15 bus to the review."
                                    :premises (list departure))))
        (test-assert (and (eq (getf (first (memory-premises note)) :kind) ':claim)
                          (eq (memory-source-tests--status configuration note) ':current))
                     "a memory derived from a derived fact is current while the fact holds")
        (test-assert (eq (memory-source-tests--error-p
                          (lambda ()
                            (memory-remember configuration
                                             :scope ':global
                                             :title "Global logistics"
                                             :content "Too broad."
                                             :premises (list departure))))
                         t)
                     "a global memory cannot rely on a workspace fact")
        (knowledge-tests--schedule-review configuration workspace "14:00")
        (multiple-value-bind (status reasons) (memory-source-tests--status configuration note)
          (test-assert (and (eq status ':stale) (equal reasons '(:premise-claim)))
                       "the memory goes stale when its fact is no longer derived"))
        (test-assert (equal (memory-assessment-summary
                             (memory-assess (memory-assessor-create configuration)
                                            (memory-find configuration (memory-identifier note))))
                            "\"leave for review at 2026-10-22 12:20 UTC\" no longer holds")
                     "the reason names the recorded fact even after it disappears"))))
  nil)

(-> knowledge-tests--recall (configuration conversation) list)
(defun knowledge-tests--recall (configuration conversation)
  "Return the contributions memory recall makes for CONVERSATION's latest input."
  (let ((*context-contributors* nil)
        (*context-resolver* (cl-llm-provider-api:make-context-resolver))
        (*context-last-deliveries* (make-hash-table :test #'equal))
        (*context-last-delivery-order* nil))
    (register-context-contributor "related-memories" 'memory-related-context
                                  :source ':built-in)
    (context-delivery-contributions (context-resolve-request configuration conversation #()))))

(-> knowledge-tests--contribution (list string) (option context-contribution))
(defun knowledge-tests--contribution (contributions identifier)
  "Return the contribution IDENTIFIER among CONTRIBUTIONS."
  (find identifier contributions :key #'context-contribution-identifier :test #'string=))


(-> test-knowledge-recall () null)
(defun test-knowledge-recall ()
  "Test recall of complete current facts and once-only explanation of their changes."
  (with-test-configuration (base root)
    (multiple-value-bind (configuration workspace)
        (memory-source-tests--workspace base root)
      (knowledge-tests--define-appointments configuration)
      (knowledge-tests--admit-departure configuration)
      (knowledge-tests--schedule-review configuration workspace "13:00")
      (knowledge-tests--said configuration "event.buffer" '("review" "PT10M")
                             "be there ten minutes early")
      (knowledge-tests--said configuration "event.travel" '("review" "PT30M")
                             "half an hour to get there")
      (let ((conversation (conversation-create configuration)))
        (conversation-append-user-message conversation "When should I leave for the review?")
        (let* ((contributions (knowledge-tests--recall configuration conversation))
               (advice (knowledge-tests--contribution contributions "related-memories")))
          (test-assert (and advice
                            (search "\"fact\":\"leave for review at 2026-10-22 12:20 UTC\""
                                    (context-contribution-evidence advice))
                            (search "\"derived\":\"departure v1\""
                                    (context-contribution-evidence advice))
                            (search "use them as they are" (context-contribution-instruction advice))
                            (not (search "persistent memor" (context-contribution-instruction advice)))
                            (null (knowledge-tests--contribution contributions "knowledge-changes")))
                       "recall offers the derived departure as a complete current fact")
          (test-assert (equal (application--related-memories-notice 0 :facts 3)
                              "@ 3 relevant facts")
                       "the notice counts offered facts"))
        (knowledge-tests--schedule-review configuration workspace "14:00")
        (test-assert (null (knowledge-tests--contribution
                            (knowledge-tests--recall configuration conversation)
                            "knowledge-changes"))
                     "changes wait for the next turn")
        (conversation-append-user-message conversation "Ok, anything else?")
        (let* ((changes (knowledge-tests--contribution
                         (knowledge-tests--recall configuration conversation)
                         "knowledge-changes")))
          (test-assert (and changes
                            (search "changed: leave for review at 2026-10-22 13:20 UTC, was leave for review at 2026-10-22 12:20 UTC"
                                    (context-contribution-evidence changes))
                            (eq (context-contribution-lifetime changes) ':next-request))
                       "the next turn explains how a fact the model was given changed")
          (test-assert (equal (context-contribution-evidence changes)
                              (context-contribution-evidence
                               (knowledge-tests--contribution
                                (knowledge-tests--recall configuration conversation)
                                "knowledge-changes")))
                       "later requests in the same turn offer the same explanation"))
        (conversation-append-user-message conversation "Thanks.")
        (test-assert (null (knowledge-tests--contribution
                            (knowledge-tests--recall configuration conversation)
                            "knowledge-changes"))
                     "each change is explained once")
        (test-assert (equal (application--knowledge-changes-notice 1)
                            "@ 1 fact given earlier changed")
                     "the person sees that a fact the model relied on changed"))))
  nil)

(-> test-knowledge-commands () null)
(defun test-knowledge-commands ()
  "Test facts in (memories), explaining and forgetting facts, (what-changed), and rules."
  (with-test-configuration (base root)
    (multiple-value-bind (configuration workspace)
        (memory-source-tests--workspace base root)
      (let ((application (make-instance 'application
                                        :configuration configuration
                                        :conversation (conversation-create configuration))))
        (flet ((text (entry)
                 "Return ENTRY's plain text."
                 (memory-source-tests--text entry)))
          (knowledge-tests--define-appointments configuration)
          (knowledge-tests--admit-departure configuration)
          (knowledge-tests--schedule-review configuration workspace "13:00")
          (knowledge-tests--said configuration "event.buffer" '("review" "PT10M")
                                 "be there ten minutes early")
          (multiple-value-bind (travel)
              (knowledge-tests--said configuration "event.travel" '("review" "PT30M")
                                     "half an hour to get there")
            (test-assert (search "facts  4" (text (application-memories-entry application)))
                         "(memories) lists facts even without prose memories")
            (test-assert (search "4 facts hold now"
                                 (text (application-knowledge-changes-entry application)))
                         "the first (what-changed) records what the person saw")
            (knowledge-tests--schedule-review configuration workspace "14:00")
            (let ((listing (text (application-memories-entry application))))
              (test-assert (and (search "facts  5 · 1 no longer holding" listing)
                                (< (search "stale: schedule.txt" listing)
                                   (search "leave for review at 2026-10-22 13:20 UTC" listing)))
                           "a fact that stopped holding is listed first with its cause"))
            (let ((changes (text (application-knowledge-changes-entry application))))
              (test-assert (and (search "~ leave for review at 2026-10-22 13:20 UTC" changes)
                                (search "was leave for review at 2026-10-22 12:20 UTC" changes))
                           "(what-changed) shows the departure moving with its cause"))
            (test-assert (search "Nothing changed since"
                                 (text (application-knowledge-changes-entry application)))
                         "(what-changed) reports each change once")
            (let* ((view (application-knowledge--view application))
                   (departure (first (knowledge-view-holds view '("event.departure" "?e" "?t"))))
                   (why (text (application-memory-entry
                               application
                               (subseq (knowledge-node-identifier departure) 0 7)))))
              (test-assert (and (search "fact  supported" why)
                                (search "derived by departure v1" why)
                                (search "observation: schedule.txt \"review at 14:00\"" why))
                           "(memory) explains a fact selected by an ID prefix"))
            (let ((forgotten (text (application-memory-forget-entry application travel))))
              (test-assert (and (search "forgot  travel to review takes 30m" forgotten)
                                (search "1 derived fact changed or stopped holding" forgotten))
                           "(forget) retracts a fact and names what stopped holding with it")))
          (test-assert (search "✓ departure v1" (text (application-knowledge-rules-entry application)))
                       "(rules) lists the admitted rule")
          (test-assert (search "retired  departure"
                               (text (application-knowledge-rule-transition-entry
                                      application "departure" ':retire)))
                       "(retire-rule) retires a rule")
          (test-assert (search "awaiting (admit-rule \"departure\")"
                               (text (application-knowledge-rules-entry application)))
                       "a retired rule waits for admission")
          (test-assert (search "admitted  departure v1"
                               (text (application-knowledge-rule-transition-entry
                                      application "departure" ':admit)))
                       "(admit-rule) admits it again")
          (test-assert (search "No rule \"nothing\" exists"
                               (text (application-knowledge-rule-transition-entry
                                      application "nothing" ':admit)))
                       "an unknown rule points to (rules)")))))
  nil)
