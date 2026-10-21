(in-package #:autolith)

;;;; -- Contract Matching --

(-> test-contract-repeated-variables () null)
(defun test-contract-repeated-variables ()
  "Repeated condition variables unify JSON values before a call matches."
  (with-test-configuration (configuration)
    (knowledge-define-contract configuration :id "same-target" :tool "resource.read"
                               :when (json-object "source" "?x" "target" "?x"))
    (dolist (values (list (list "a" "b" nil)
                         (list "a" "a" t)
                         (list "A" "a" nil)
                         (list (vector (json-object "id" "a"))
                               (vector (json-object "id" "a")) t)
                         (list (json-object "id" "a") (json-object "id" "b") nil)
                         (list nil nil t)
                         (list 1 1.0 t)))
      (destructuring-bind (source target expected) values
        (let ((matches (knowledge-contracts-for-call
                        configuration "resource.read"
                        (json-object "source" source "target" target))))
          (test-assert (eq expected (not (null matches)))
                       "repeated variables compare actual JSON values")
          (when matches
            (test-assert (= 1 (length (second (first matches))))
                         "a repeated variable has a single binding")))))
    (test-assert (null (knowledge-contracts-for-call
                        configuration "resource.read" (json-object "source" "a")))
                 "a missing repeated argument cannot match"))
  nil)
