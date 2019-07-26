;; Set of commands for ending a session/locking the screen.
(in-package :stumpwm)
(defcommand session-lock () ()
  (run-shell-command "slock"))

(defcommand session-suspend () ()
  (session-lock)
  (run-shell-command "systemctl suspend"))

(defun close-all-apps ()
  "Closes all windows managed by stumpwm gracefully"
  (let ((win-index-text (run-shell-command "wmctrl -l | awk '{print $1}'" t)))
    (dolist (window (cl-ppcre:split "\\\n" win-index-text))
      (run-shell-command (format nil "wmctrl -i -c ~A" window)))))

(defun session-clean ()
  "Prepares session for quiting"
  (close-all-apps)
  (run-hook *quit-hook*))

(defcommand session-reboot () ()
  (session-clean)
  (run-shell-command "systemctl reboot"))

(defcommand session-power-off () ()
  (session-clean)
  (run-shell-command "systemctl poweroff"))

(defcommand session-end () ()
  (session-clean)
  (quit))

(defparameter *session-map*
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "l") "session-lock")
    (define-key m (kbd "s") "session-suspend")
    (define-key m (kbd "r") "session-reboot")
    (define-key m (kbd "p") "session-power-off")
    (define-key m (kbd "e") "session-end")
    m) )

(define-key *root-map* (kbd "s") *session-map*)
