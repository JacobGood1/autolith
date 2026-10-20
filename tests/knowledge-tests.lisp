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
