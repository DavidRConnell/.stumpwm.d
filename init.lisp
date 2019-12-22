;;; -*-  mode: lisp; -*-
;;; Based of tpine's stumpwm configuration.

;;; Load extra packages
(ql:quickload :cl-utilities)
(ql:quickload :clx-truetype)
(ql:quickload :slynk)

(in-package :stumpwm)

(set-prefix-key (kbd "C-t"))

;;; Load Slynk
(slynk:create-server :port 4004
                     :dont-close t)

(setq *startup-message* (format nil "Welcome David~%Slynk is on port 4004~%Happy Hacking!"))

;;; Startup Programs
(run-shell-command "redshift")
(run-shell-command "emacs --daemon")

;; set module directory (if not already set)
(set-module-dir "~/projects/stumpwm-contrib/")
(defvar *config-dir* "~/.stumpwm.d/")
(defun load-config-file (relative-path)
  (load (concat *config-dir* relative-path)))

(load-config-file "general.lisp")
(load-config-file "profile.lisp")
(load-config-file "secrets.lisp") ; Defines vars I don't want to share.
(load-config-file "session.lisp")
(load-config-file "media.lisp")
(load-config-file "visual.lisp")
(load-config-file "buffers.lisp")
(load-config-file "find-file")
(load-config-file "apps.lisp")
(load-config-file "local.lisp")
