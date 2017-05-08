
# emacs-nodejs-slave

Run JS code from emacs lisp (in a slave node.js process)

## Install

### NodeJS part

Install [eval-slave](https://github.com/futpib/eval-slave) globally (elisp code needs `node-eval-slave` executable):

```
$ npm install -g eval-slave
```

### Emacs part

Install with [cask](https://cask.github.io/):

```lisp
(depends-on "nodejs-slave" :git "https://github.com/futpib/nodejs-slave.git")
```

```
$ cask install
```

Will be on melpa someday.

## Example

### Require things

```lisp
(require 'nodejs-slave)
(require 'promise) ; we are goint to use promises to simplify the async deal
;; you can also use `async-await` if you prefer so
```

(emacs-promise)[https://github.com/chuntaro/emacs-promise]
(emacs-async-await)[https://github.com/chuntaro/emacs-async-await]

### Run with build-in (rudimentary) s-exp to js compiler

```lisp
(promise-done (promise-chain
 (nodejs-slave-run
  '(lambda (uptime)
     (new Promise
          (lambda (resolve)
            (let ((util (require "util")))
              (setTimeout (lambda ()
                            (resolve (util.format
                                      "No way! Your emacs is running %s!"
                                      uptime)))
                          500)))))
  (emacs-uptime))
 (then (lambda (result)
         (print result)))))

;; Prints something like this in about 500 milliseconds:
;; "No way! Your emacs is running 2 days, 7 hours, 9 minutes, 1 second!"
```

Check out (tests)[https://github.com/futpib/emacs-nodejs-slave/blob/master/test/emacs-nodejs-slave-test.el] if you wonder what exactly is supported by the JS DSL (contributions are wellcome)

### Run js string

```lisp
(promise-done (promise-chain
 (nodejs-slave-run
  "uptime => new Promise(resolve => {
    let util = require('util');
        setTimeout(() => {
            resolve(util.format(\"No way! Your emacs is running %s!\", uptime))
        }, 500)
    })"
  (emacs-uptime))
 (then (lambda (result)
         (print result)))))
```

Note that the above js code is much nicer than what the compiled s-exp version will look like, so you might prefer js source strings for debugging reasons

## Limitations

Arguments and return values are marshaled with JSON, so you have to make sure you pass and return plain JSON objects.