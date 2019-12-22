;;; Load package
(in-package :stumpwm)

;; Volume control
; TODO: create a volume command macro.
(defcommand vol-inc (amount) ((:number "Increase volume by: "))
  (run-shell-command (format nil "pulseaudio-ctl up ~a" amount)))

(defcommand vol-dec (amount) ((:number "Decrease volume by: "))
  (run-shell-command (format nil "pulseaudio-ctl down ~a" amount)))

(defcommand vol-set (amount) ((:number "Set volume to: "))
  (run-shell-command (format nil "pulseaudio-ctl set ~a" amount)))

(defcommand mute () ()
  (run-shell-command "pulseaudio-ctl mute"))

(define-key *top-map* (kbd "XF86AudioLowerVolume") "vol-dec 5")
(define-key *top-map* (kbd "XF86AudioRaiseVolume") "vol-inc 5")
(define-key *top-map* (kbd "XF86AudioMute") "mute")

(define-interactive-keymap *interactive-volume-map* ()
  ((kbd "k") "vol-inc 1")
  ((kbd "j") "vol-dec 1")
  ((kbd "K") "vol-inc 10")
  ((kbd "J") "vol-dec 10")
  ((kbd "m") "mute")
  ((kbd "s") "vol-set"))

(define-key *root-map* (kbd "v") "*interactive-volume-map*")

;; Screen control
(defun parse-xrandr-output (output)
  "Really ugly TODO: clean-up"
  (let* ((split-output (split-string output (format nil "~C" #\Space)))
         (pos (split-string (first
                             (remove-if-not
                              #'(lambda (x) (string-match x "\\\+.*\\\+.*"))
                              split-output))
                            "+"))
         (table (make-hash-table)))
    (setf (gethash 'name table) (first split-output))
    (setf (gethash 'x table) (second pos))
    (setf (gethash 'y table) (third pos))
    table))

(defun list-xrandr-screens ()
    (loop for screen in (run-shell-command-list
                         "xrandr -q | awk '/\\<connected\\>/'") collecting
                           (parse-xrandr-output screen)))

(defun get-head-xrandr-name (&optional (head (current-head)))
  (let ((x (format nil "~a" (head-x head)))
        (y (format nil "~a" (head-y head))))
    (loop for xrandr-screen in (list-xrandr-screens) do
      (if (and
           (string= x (gethash 'x xrandr-screen))
           (string= y (gethash 'y xrandr-screen)))
          (return (gethash 'name xrandr-screen))))))
