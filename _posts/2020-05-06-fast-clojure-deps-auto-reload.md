---
layout: post
title: Fast Clojure dependency hot-loading
description: Using Emacs and Pomegranate to dynamically import Clojure dependencies
summary: A Pomegranate CIDER cocktail
tags: [clojure,emacs]
---


## About Clojure

Clojure is great. I love it.

But like most great things it has its downsides.

One of which is the startup time.

There are workarounds to this[^1] but the lost common solution is to try avoiding as much possible to restart your process.

That's not too much of an issue as Clojure was built with interactive development in mind (hello Mr REPL) and additional tooling (such as [CIDER](https://cider.mx/)) push things a step further.

But there is one case when you still usually have to restart the REPL: when adding dependencies.


## Pomegranate

[Pomegranate](https://github.com/clj-commons/pomegranate) is a library that allows to import dependencies without having to restart your process.

You of course first need to have it in your project dependencies[^2]:

```clojure
(defproject pomegranate-test "0.1.0-SNAPSHOT"
  ;; ...
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [clj-commons/pomegranate "1.2.0"]])
```

Then, let's say you wanted to import `clj-http`. In your REPL, you just have to do the following:

```clojure
(use '[cemerick.pomegranate :only (add-dependencies)])
(add-dependencies :coordinates '[[clj-http "3.10.1"]]
                  :repositories (merge cemerick.pomegranate.aether/maven-central
                                       {"clojars" "https://clojars.org/repo"}))
```


## youWereExpectingMeToTypeAllThis.png

That's a big hunk of text to copy paste...

What could we do to make our life easier?

Mhm, wait a second, we are using Emacs with CIDER, don't we?

Let's hack something quick in elisp!


## Emacs helper command

```emacs-lisp
(defun prf/cider/send-to-repl (sexp &optional eval ns)
  "Send SEXP to Cider Repl. If EVAL is t, evaluate it.
Optionally, we can change namespace by specifying NS."
  (cider-switch-to-repl-buffer ns)
  (goto-char cider-repl-input-start-mark)
  (delete-region (point) (point-max))
  (save-excursion
    (insert sexp)
    (when (equal (char-before) ?\n)
      (delete-char -1)))
  (when eval
    (cider-repl--send-input t)))

(defun prf/clj/pomegranate-dep (dep)
  "Format a Clojure Pomegranate dependency import for DEP."
  (concat
   (format
    "%s"
    ;; NB: this is clojure!
    `(use '[cemerick.pomegranate :only (add-dependencies)]))
   (s-replace-all
    `(("\\." . ".")
      ("mydep" . ,dep))
    (format
     "%S"
     ;; NB: this is clojure!
     `(add-dependencies :coordinates '[mydep]
                        :repositories (merge cemerick.pomegranate.aether/maven-central
                                             {"clojars" "https://clojars.org/repo"}))))))

(defun prf/cider/inject-pomegranate-dep (&optional dep ns)
  "Auto-import DEP in the current Clojure Repl using Pomegranate.
Optionally, we can change namespace by specifying NS."
  (interactive)
  (setq dep (or dep (read-string "Dep: ")))
  (prf/cider/send-to-repl (prf/clj/pomegranate-dep dep) t ns))
```

Now, from any project buffer (Clojure source file of REPL), we can just:

```
M-x prf/cider/inject-pomegranate-dep
Dep: [clj-http "3.10.1"]
```

And this should auto-switch to our running REPL buffer and let Pomegranate do its magic!


## Alternative implementation: cljr-refactor

The excellent [clj-refactor](https://github.com/clojure-emacs/clj-refactor.el) package provides the command `cljr-hotload-dependency` (`<prefix> + hd`) that does the same thing. It also provides command `add-project-dependency` (`<prefix> + ap`) that does the same but also adds it to your project.clj file.


## Notes

[^1]: Like compiling to a [GraalVM native image](https://www.graalvm.org/docs/reference-manual/native-image/) or using the [AppCDS feature of commercial Oracle JVM](http://blog.gilliard.lol/2017/10/04/AppCDS-and-Clojure.html).

[^2]: This is unecessary when using the CIDER REPL in Emacs, as Pomegranate already gets side-loaded in this case
