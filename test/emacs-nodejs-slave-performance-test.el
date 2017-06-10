;;; -*- lexical-binding: t -*-

(defun file-string (filename)
  "Return the contents of FILENAME."
  (with-temp-buffer
    (insert-file-contents filename)
    (buffer-string)))

(ert-deftest test/js/benchmark/chars-parser ()
  (let* ((parser (nodejs-slave--make-strings-to-chars (lambda (x) nil)))
         (string (file-string (f-join root-test-path "fixtures" "input.json")))
         (benchmark (benchmark-run 1 (funcall parser string)))
         (wall-time (first benchmark))
         (gc-runs (second benchmark))
         (gc-time (third benchmark)))
    (princ-list "chars benchmark: " benchmark)
    ;; (should (< gc-runs 10))
    ;; (should (< gc-time (/ wall-time 10)))
    ))

(ert-deftest test/js/benchmark/chars+json-parser ()
  (let* ((parser (nodejs-slave--make-strings-to-chars
                  (nodejs-slave--make-json-stream-parser (lambda (name char) nil))))
         (string (file-string (f-join root-test-path "fixtures" "input.json")))
         (benchmark (benchmark-run 1 (funcall parser string)))
         (wall-time (first benchmark))
         (gc-runs (second benchmark))
         (gc-time (third benchmark)))
    (princ-list "chars+json benchmark: " benchmark)
    ;; (should (< gc-runs 10))
    ;; (should (< gc-time (/ wall-time 10)))
    ))

(ert-deftest test/js/benchmark/objarr-parser ()
  (let* ((parser (nodejs-slave--make-json-objects-array-stream-parser (lambda (x) nil)))
         (string (file-string (f-join root-test-path "fixtures" "input.json")))
         (benchmark (benchmark-run 1 (funcall parser string)))
         (wall-time (first benchmark))
         (gc-runs (second benchmark))
         (gc-time (third benchmark)))
    (princ-list "array of objects benchmark: " benchmark)
    ;; (should (< gc-runs 10))
    ;; (should (< gc-time (/ wall-time 10)))
    ))
