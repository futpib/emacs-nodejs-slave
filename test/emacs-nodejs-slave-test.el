;;; -*- lexical-binding: t -*-

(ert-deftest test/js/nil ()
  (should (equal (nodejs-slave-js '()) "undefined")))

(ert-deftest test/js/identifier ()
  (should (equal (nodejs-slave-js 'a) "a"))
  (should (equal (nodejs-slave-js 'a.m) "a.m")))

(ert-deftest test/js/number ()
  (should (equal (nodejs-slave-js 1.234) "1.234")))

(ert-deftest test/js/string ()
  (should (equal (nodejs-slave-js "a") "\"a\"")))

(ert-deftest test/js/array ()
  (should (equal (nodejs-slave-js [1 2 3]) "[1,2,3]")))

(ert-deftest test/js/object ()
  (should (equal (nodejs-slave-js '(:a 1 :b 2 :c 3)) "{\"a\":1,\"b\":2,\"c\":3}")))

(ert-deftest test/js/call ()
  (should (equal (nodejs-slave-js '(a b)) "(a)(b)"))
  (should (equal (nodejs-slave-js '(a.m b)) "(a.m)(b)")))

(ert-deftest test/js/return ()
  (should (equal (nodejs-slave-js '(return a)) "return a"))
  (should (equal (nodejs-slave-js '(return nil)) "return undefined")))

(ert-deftest test/js/throw ()
  (should (equal (nodejs-slave-js '(throw a)) "throw a"))
  (should (equal (nodejs-slave-js '(throw nil)) "throw undefined")))

(ert-deftest test/js/new ()
  (should (equal (nodejs-slave-js '(new Array 1 2 3)) "new Array(1, 2, 3)"))
  (should (equal (nodejs-slave-js '(new Object)) "new Object()")))

(ert-deftest test/js/lambda-empty ()
  (should (equal (nodejs-slave-js '(lambda ())) "function () {return undefined;}")))

(ert-deftest test/js/lambda-nil ()
  (should (equal (nodejs-slave-js '(lambda () ())) "function () {return undefined;}")))

(ert-deftest test/js/lambda-one ()
  (should (equal (nodejs-slave-js '(lambda () a)) "function () {return a;}")))

(ert-deftest test/js/lambda-two ()
  (should (equal (nodejs-slave-js '(lambda () a b)) "function () {a;return b;}")))

(ert-deftest test/js/lambda-one-arg ()
  (should (equal (nodejs-slave-js '(lambda (a))) "function (a) {return undefined;}")))

(ert-deftest test/js/lambda-two-arg ()
  (should (equal (nodejs-slave-js '(lambda (a b) ())) "function (a, b) {return undefined;}")))

(ert-deftest test/js/progn ()
  (should
   (equal
    (nodejs-slave-js '(progn
           (console.log "a")
           (console.log "b")))
    "(function () {(console.log)(\"a\");return (console.log)(\"b\");})()")))

(ert-deftest test/js/let ()
  (should
   (equal
    (nodejs-slave-js '(let ((a 1)
               (b 2))
           (console.log a b)))
    "(function (a, b) {return (console.log)(a, b);})(1, 2)")))

(ert-deftest test/js/setq ()
  (should (equal (nodejs-slave-js '(setq a b)) "a = b")))

(ert-deftest test/js/unwind-protect ()
  (should (equal
           (nodejs-slave-js '(unwind-protect (run) (fin1) (fin2)))
           "try { (run)() } finally { (function () {(fin1)();return (fin2)();})() }")))

(ert-deftest test/js/condition-case ()
  (should (equal
           (nodejs-slave-js '(condition-case err protectedForm (error handler)))
           "try { protectedForm } catch (err) { handler }")))


(ert-deftest test/js/strings-to-chars ()
  (let* ((consumer-calls '())
         (consumer (lambda (c) (push c consumer-calls)))
         (str2char (nodejs-slave--make-strings-to-chars consumer)))
    (funcall str2char "top")
    (funcall str2char "kek")
    (should (equal (reverse consumer-calls) '("t" "o" "p" "k" "e" "k")))))

(defmacro def-json-test (string expected-calls)
  `(ert-deftest ,(intern (concat "test/js/json-stream-parser/" string)) ()
     (let* ((consumer-calls '())
            (consumer (lambda (tag char) (push (list tag char) consumer-calls)))
            (json-from-chars
             (nodejs-slave--make-json-stream-parser consumer))
            (json-from-strings
             (nodejs-slave--make-strings-to-chars json-from-chars)))
       (funcall json-from-strings ,string)
       (should
        (equal
         (reverse consumer-calls)
         ,expected-calls)))))

(def-json-test
  "[]"
  '((array-start "[")
    (array-end "]")))

(def-json-test
  "[\"foo\"]"
  '((array-start "[")
    (string-start "\"")
    (nil "f")
    (nil "o")
    (nil "o")
    (string-end "\"")
    (array-end "]")))

(def-json-test
  "{}"
  '((object-start "{")
    (object-end "}")))

(def-json-test
  "\"[{}]\""
  '((string-start "\"")
    (nil "[")
    (nil "{")
    (nil "}")
    (nil "]")
    (string-end "\"")))

(ert-deftest test/js/objects-array-stream ()
  (let* ((consumer-calls '())
         (consumer (lambda (o) (push o consumer-calls)))
         (objects (nodejs-slave--make-json-objects-array-stream-parser consumer))
         (obj-1 '((foo . ((bar . "buz")))))
         (obj-2 '((top . ((kek . "lul"))))))
    (funcall objects "[")
    (funcall objects (json-encode obj-1))
    (funcall objects ",")
    (funcall objects (json-encode obj-2))
    (funcall objects "]")
    (should
     (equal
      consumer-calls
      (reverse (list obj-1 obj-2))))))


(async-ert-deftest test/js/run-sync
  (let ((result (await
                 (nodejs-slave-run '(lambda () "test")))))
    (should (equal result "test"))))

(async-ert-deftest test/js/run-string
  (let ((result (await
                 (nodejs-slave-run "() => \"test\""))))
    (should (equal result "test"))))

(async-ert-deftest test/js/run-promise
  (let ((result (await
                 (nodejs-slave-run
                  '(lambda () (new Promise (lambda (resolve) (resolve "test"))))))))
    (should (equal result "test"))))

(async-ert-deftest test/js/run-throw
  (condition-case err
      (let ((result (await
                     (nodejs-slave-run '(lambda () (throw "kek") ())))))
        (error "This should be unreachable"))
      (error
       (should (equal (error-message-string err) "kek")))))

(async-ert-deftest test/js/process-died-and-restarted
  (condition-case err
      (let ((result (await
                     (nodejs-slave-run "() => process.exit(0)"))))
        (error "This should be unreachable"))
    (error
     (should (equal (await (nodejs-slave-run "() => 1")) 1)))))

(ert-deftest-async
 test/js/run-example
 (done)
 (promise-done
  (promise-chain
      (nodejs-slave-run
       '(lambda (uptime)
          (new Promise (lambda (resolve)
                         (let ((util (require "util")))
                           (setTimeout (lambda ()
                                         (resolve (util.format
                                                   "No way! Your emacs is running %s!"
                                                   uptime)))
                                       1)))))
       (emacs-uptime))
    (then (lambda (value)
            (should (equal value "No way! Your emacs is running 0 seconds!"))))
    ) (lambda (v) (funcall done)) done))

(ert-deftest-async
 test/js/run-example-string
 (done)
 (promise-done
  (promise-chain
      (nodejs-slave-run
       "uptime => new Promise(resolve => {
            let util = require('util');
            setTimeout(() => {
                resolve(util.format(\"No way! Your emacs is running %s!\", uptime))
            }, 1)
        })"
       (emacs-uptime))
    (then (lambda (value)
            (should (equal value "No way! Your emacs is running 0 seconds!"))))
    ) (lambda (v) (funcall done)) done))
