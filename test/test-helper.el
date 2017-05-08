;;; -*- lexical-binding: t -*-

(require 'ert-async)
(require 'promise)
(require 'f)

(defvar root-test-path
  (f-dirname (f-this-file)))

(defvar root-code-path
  (f-parent root-test-path))

(require 'nodejs-slave (f-expand "nodejs-slave.el" root-code-path))

;; (font-lock-add-keywords
;;  nil
;;  '(("(\\(\\<async-ert-deftest\\)\\>\\s *\\(\\(?:\\sw\\|\\s_\\)+\\)?"
;;     (1 font-lock-keyword-face nil t)
;;     (2 font-lock-function-name-face nil t))))

;; FIXME: hygiene
(defmacro async-ert-deftest (name &rest body)
  "Define async test powered by `ert-async` and `async-await`."
  `(ert-deftest-async
    ,name
    (async-ert-deftest--callback)
    (promise-done
     (funcall
      (async-lambda
        ()
        (await (funcall (async-lambda () ,@body)))
        (funcall async-ert-deftest--callback))))))
