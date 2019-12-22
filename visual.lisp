(in-package :stumpwm)

(let ((bg "#282c34")
      (fg "#a9a1e1"))

  (set-fg-color fg)
  (set-bg-color "#000000")
  (set-border-color bg)
  (set-focus-color fg)
  (set-unfocus-color bg)

  (setf *mode-line-foreground-color* bg
        *mode-line-border-width* 0
        *mode-line-background-color* fg
        *mode-line-highlight-template* "^B~A^b")

  (setf *grab-pointer-character* 40
        *grab-pointer-character-mask* 41)

	
  (setf (car *colors*) bg
	(car (last *colors*)) fg)
  (update-color-map (current-screen)))

(set-font "-*-DejaVuSansMono Nerd Font-*-r-*-*-15-*-*-*-*-*-*-*")
(set-msg-border-width 0)
(setf *input-window-gravity* :center
      *message-window-gravity* :center
      *message-window-padding* 15
      *message-window-y-padding* 20)

(setf *menu-maximum-height* 15)

;; Mode line
(defvar *mode-line-sep* " | "
  "Symbol for seperating elements on the mode line")

(defun get-battery-info ()
  (let* ((battery-stats (run-shell-command-list
                         "upower -i /org/freedesktop/UPower/devices/battery_BAT0 | awk -F\": *\" '/(percentage|state):/ { print $2 }'"))
         (state (first battery-stats))
         (percent (second battery-stats))
         (charge-symbol (if (string= "discharging" state)
                            ""
                            (format nil "~C" #\U+26a1)))

         (percent-int (parse-integer (string-trim "%" percent)))
         (battery-symbol (format nil "~C"
                                 (cond ((> percent-int 98) #\U+f578)
                                       ((> percent-int 90) #\U+f581)
                                       ((> percent-int 80) #\U+f580)
                                       ((> percent-int 70) #\U+f57f)
                                       ((> percent-int 60) #\U+f57e)
                                       ((> percent-int 50) #\U+f57d)
                                       ((> percent-int 40) #\U+f57c)
                                       ((> percent-int 30) #\U+f57b)
                                       ((> percent-int 20) #\U+f57a)
                                       (t #\U+f579)))))
    (cond ((< percent-int 10)
           (setf battery-symbol (concat "^[^1*" battery-symbol "^]")))
          ((< percent-int 30)
           (setf battery-symbol (concat "^[^3*" battery-symbol "^]"))))

    (if (emptyp percent)
        ""
        (format nil "~A~A ~A~A" charge-symbol battery-symbol percent *mode-line-sep*))))

(defun get-volume ()
  (let* ((volume-stats (split-string
                        (run-shell-command "pulseaudio-ctl full-status" t)
                        " "))
         (percent-string (first volume-stats))
         (percent-int (parse-integer percent-string))
         (sign (cond ((string= (second volume-stats) "yes")
                      #\U+fc5d)
                     ((> percent-int 50)
                      #\U+f028)
                     (t #\U+f027))))

    (format nil "~C ~A" sign percent-string)))

(defun get-mail ()
  (let ((emails (remove #\Newline (run-shell-command "mu find g:n | wc -l" t))))
    (if (equal emails "0")
        (format nil "~C~A" #\U+faee *mode-line-sep*)
        (format nil "^[^3*~C ~A^]~A" #\U+f6ed emails *mode-line-sep*))))

(setf *screen-mode-line-format*
      (list " "
            "%d"
            *mode-line-sep*
            "%W"
            "%u"
            "^>"
            *mode-line-sep*
            '(:eval (get-mail))
            '(:eval (get-battery-info))
            '(:eval (get-volume))
            " "))

(setf *mode-line-pad-x* 0)
(setf *time-modeline-string*
      "%a %b %e %R")

(setf *window-format*
      "%n: %12c")

(defcommand toggle-all-mode-lines () ()
    (loop for head in (screen-heads (current-screen)) do
      (toggle-mode-line (current-screen) head)))

(toggle-all-mode-lines)
(define-key *root-map* (kbd "\\") "toggle-all-mode-lines")
