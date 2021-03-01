;;; Add general purpose functions/commands/macros here.
(in-package :stumpwm)

(defun restart-slynk ()
  "Restart Slynk and reload source.
This is needed if Sly updates while StumpWM is running"
  (slynk:stop-server 4004)
  (slynk:create-server :port 4004
                       :dont-close t))

(defun run-shell-command-list (command)
  "Run shell command and return a list split at newline characters."
  (split-string (run-shell-command command t)
                (format nil "~C" #\Newline)))

(defun restart-volume ()
  (run-shell-command "kill $(ps -e | awk '/pulseaudio$/ { print $1 }')" t)
  (run-shell-command "pulseaudio --start" t))
