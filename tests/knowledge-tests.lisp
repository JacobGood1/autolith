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
