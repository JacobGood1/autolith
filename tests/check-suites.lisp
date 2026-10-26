(in-package #:autolith)

;;;; -- Source Check Catalog --

;;; The source checks and their Mallet and parinfer tools are development-only,
;;; so their suite loads with autolith/check-tests rather than autolith/tests.

(define-test-suite source-checks
  test-source-syntax-positions
  test-source-rule-findings
  test-source-rule-repairs
  test-source-check-repair-guard
  test-source-check-repository-run
  test-source-parinfer-missing-library
  test-source-mallet-findings
  test-source-warning-run
  test-source-warning-ownership
  test-source-system-platform-support)
