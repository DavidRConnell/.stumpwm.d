;;; Add general purpose functions/commands/macros here.
(in-package :stumpwm)

(defun run-shell-command-list (command)
  "Run shell command and return a list split at newline characters."
  (split-string (run-shell-command command t)
                (format nil "~C" #\Newline)))
