;;; nodejs-slave.el --- Node.js slave evaluator                   -*- lexical-binding: t; -*-

;; Copyright (C) 2017  futpib

;; Author: futpib <futpib@gmail.com>
;; URL: https://github.com/gmail/emacs-nodejs-slave
;; Package-Requires: ((emacs "25") (promise "1.0"))
;; Version: 1.0
;; Keywords: node js es nodejs eval

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; TODO

;;; Code:

(require 'cl)
(require 'json)
(require 'promise)
(require 'async-await)


(defun nodejs-slave-js (source)
  "Turn s-expressions to JS code."
  (let ((head (car-safe source))
        (tail (cdr-safe source)))
    (cond

     ((equal head 'lambda)              ; lambda
      (let ((args (car-safe tail))
            (body (cdr-safe tail)))
        (concat
         "function ("
         (mapconcat
          (lambda (e) (format "%s" e))
          args
          ", ")
         ") {"
         (mapconcat
          (lambda (e) (format "%s;" e))
          (let ((butlast (butlast body))
                (last (car-safe (last body))))
            (append
             (mapcar 'nodejs-slave-js butlast)
             (list (nodejs-slave-js `(return ,last)))))
          "")
         "}")))

     ((equal head 'return)
      (format "return %s" (nodejs-slave-js (car-safe tail))))

     ((equal head 'throw)
      (format "throw %s" (nodejs-slave-js (car-safe tail))))

     ((equal head 'new)
      (let ((constructor
             (nodejs-slave-js (car-safe tail)))
            (arguments
             (mapconcat 'nodejs-slave-js (cdr-safe tail) ", ")))
        (format "new %s(%s)" constructor arguments)))

     ((equal head 'progn)               ; progn
      (nodejs-slave-js
       `((lambda ()
           ,@tail))))

     ((equal head 'unwind-protect)      ; unwind-protect
      (let ((try (car-safe tail))
            (fin (cdr-safe tail)))
        (format "try { %s } finally { %s }"
                (nodejs-slave-js try)
                (nodejs-slave-js `(progn ,@fin)))))

     ((equal head 'condition-case)      ; condition-case
      (let* ((var (first tail))
             (body (second tail))
             (handler (first (cdr-safe (cdr-safe tail))))
             (handler-condition (first handler)) ; ignored
             (handler-body (second handler)))
        (format "try { %s } catch (%s) { %s }"
                (nodejs-slave-js body)
                var
                (nodejs-slave-js handler-body))))

     ((equal head 'signal)              ; signal
      (let* ((error-constructor (first tail))
             (error-arguments (cdr-safe tail)))
        (format "throw %s;"
                (nodejs-slave-js `(new ,error-constructor ,@error-arguments)))))

     ((equal head 'eq)                  ; eq
      (let ((a (first tail))
            (b (second tail)))
        (format "(%s === %s)" (nodejs-slave-js a) (nodejs-slave-js b))))

     ((equal head 'slot-value)          ; slot-value
      (let ((object (first tail))
            (name (second (second tail))))
        (format "(%s).%s" (nodejs-slave-js object) (nodejs-slave-js name))))

     ((equal head 'let)                 ; let
      (let ((pairs (car-safe tail)))
        (let ((names (mapcar 'first pairs))
              (values (mapcar 'second pairs))
              (body (cdr-safe tail)))
          (nodejs-slave-js
           `((lambda ,names ,@body) ,@values)))))

     ((equal head 'setq)                ; setq
      (let ((name (first tail))
            (value (second tail)))
        (format "%s = %s" name (nodejs-slave-js value))))

     ((equal nil source)                ; nil
      "undefined")

     ((symbolp source)                  ; identifier
      (symbol-name source))

     ((stringp source)                  ; string
      (json-encode source))

     ((numberp source)                  ; number
      (json-encode source))

     ((arrayp source)                   ; array
      (json-encode source))

                                        ; object
     ((and
       (json-plist-p source)
       (not (equal nil source)))
      (json-encode source))

     ((listp source)                    ; application
      (format "(%s)(%s)" (nodejs-slave-js head) (mapconcat 'nodejs-slave-js tail ", ")))

     (t ""))))                          ; dunno


(defun nodejs-slave--make-strings-to-chars (consumer)
  (lambda (string)
    (mapcar consumer string)))

(defun nodejs-slave--make-json-stream-parser (consumer)
  (let ((stack '())
        (prev-char nil))

    (lambda (char)
      (cond

       ((and (equal (first stack) 'string)
             (equal char ?\")
             (not (equal prev-char ?\\)))

        (funcall consumer 'string-end char)
        (pop stack))

       ((equal (first stack) 'string)

        (funcall consumer nil char))

       ((equal char ?\")

        (funcall consumer 'string-start char)
        (push 'string stack))

       ((equal char ?\[)

        (funcall consumer 'array-start char)
        (push 'array stack))

       ((equal char ?\])

        (funcall consumer 'array-end char)
        (pop stack))

       ((equal char ?\{)

        (funcall consumer 'object-start char)
        (push 'object stack))

       ((equal char ?\})

        (funcall consumer 'object-end char)
        (pop stack))

       (t

        (funcall consumer nil char)))

      (setq prev-char char))))

(defun nodejs-slave--make-json-objects-array-stream-parser (consumer)
  (let ((in-array nil)
        (in-object-depth 0)
        (chars-stack nil))
    (cl-flet ((object-append (char) (push char chars-stack))
              (object-yield ()
                            (funcall consumer
                                     (json-read-from-string
                                      (apply 'string (reverse chars-stack))))
                            (setq chars-stack nil)))
      (nodejs-slave--make-strings-to-chars
       (nodejs-slave--make-json-stream-parser
        (lambda (name char)
          (cond

           ((and in-array
                 (equal name 'object-start))
            (incf in-object-depth)
            (object-append char))

           ((and (> in-object-depth 0)
                 (equal name 'object-end))
            (decf in-object-depth)
            (object-append char)
            (when (= in-object-depth 0)
              (object-yield)))

           ((> in-object-depth 0)
            (object-append char))

           ((equal name 'array-start)
            (setq in-array t))

           ((equal name 'array-end)
            (setq in-array nil)))))))))

(define-error 'nodejs-slave-process-died
  "Slave `node` process died")

(defun nodejs-slave--make-process ()
  "Make a 'slave' nodejs process."
  (let ((output-reader (nodejs-slave--make-json-objects-array-stream-parser
                        (lambda (obj)
                          (let* ((type (alist-get 'type obj))
                                 (data (alist-get 'data obj))
                                 (id (alist-get 'id data))
                                 (value (alist-get 'value data))
                                 (reason (alist-get 'reason data)))
                            (cond ((equal type "fulfilled")
                                   (nodejs-slave--resolve-task id value))
                                  ((equal type "rejected")
                                   (nodejs-slave--reject-task id reason))))))))
    (make-process
     :name "nodejs-slave `node` process"
     :command '("node-eval-slave")

     :filter (lambda (proc string)
               (funcall output-reader string))

     :sentinel (lambda (proc event)
                 (when (not (process-live-p proc))
                   (condition-case err  ; TODO: simpler way to create error?
                       (signal 'nodejs-slave-process-died event)
                     (nodejs-slave-process-died
                      (maphash (lambda (id value)
                                 (pcase-let ((`(,resolve ,reject) value))
                                   (funcall reject err)))
                               nodejs-slave--tasks-hash)))))

     :noquery t)))

(defvar nodejs-slave--process nil)

(defun nodejs-slave--init ()
  (cl-flet ((restart ()
               (setq nodejs-slave--process (nodejs-slave--make-process))
               (process-send-string nodejs-slave--process "[")))
    (cond ((not nodejs-slave--process)
           (restart))
          ((not (process-live-p nodejs-slave--process))
           (delete-process nodejs-slave--process)
           (restart)))))

(defvar nodejs-slave--task-id 0)
(defconst nodejs-slave--tasks-hash (make-hash-table))

(defun nodejs-slave--resolve-task (id value)
  (pcase-let ((`(,resolve ,reject) (gethash id nodejs-slave--tasks-hash)))
    (funcall resolve value)
    (remhash id nodejs-slave--tasks-hash)))

(defun nodejs-slave--reject-task (id reason)
  (pcase-let ((`(,resolve ,reject) (gethash id nodejs-slave--tasks-hash)))
    (funcall reject reason)
    (remhash id nodejs-slave--tasks-hash)))

(defun nodejs-slave--run (code args resolve reject)
  (nodejs-slave--init)
  (let* ((id (incf nodejs-slave--task-id))
         (json (json-encode
                (list
                 :id id
                 :source code
                 :arguments args))))
    (puthash id (list resolve reject) nodejs-slave--tasks-hash)
    (process-send-string nodejs-slave--process json)
    ;; FIXME, HACK: \n is used to flush the pipe
    (process-send-string nodejs-slave--process ",\n")))

(defun nodejs-slave-run (code &rest args)
  "Run js CODE in a `node` process, return promise of evaluation result."
  (when (listp code)
    (setq code (nodejs-slave-js code)))
  (promise-new (lambda (resolve reject)
                 (nodejs-slave--run code (apply 'vector args) resolve reject))))

(provide 'nodejs-slave)
;;; nodejs-slave.el ends here
