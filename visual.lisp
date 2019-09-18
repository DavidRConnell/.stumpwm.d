(in-package :stumpwm)

;;; Visual
(let ((bg "#282a36")
      (fg "#8986d1"))

  (set-fg-color fg)
  (set-bg-color bg)
  (set-border-color fg)
  (set-focus-color fg)
  (set-unfocus-color bg)

  (setf *mode-line-foreground-color* fg
	*mode-line-background-color* bg
	*mode-line-border-color* fg)
	
  (setf (car *colors*) bg
	(car (last *colors*)) fg)
  (update-color-map (current-screen)))

(ql:quickload "clx-truetype")
(load-module "ttf-fonts")
(xft:cache-fonts)
(set-font (make-instance 'xft:font
                         :family "DejaVuSansMono Nerd Font"
                         :subfamily "Book"
                         :size 12))

;; Mode line

(defvar *mode-line-sep* " | "
  "Symbol for seperating elements on the mode line")

(defun get-battery-info ()
  (let* ((battery-stats (run-shell-command-list
                         "upower -i /org/freedesktop/UPower/devices/battery_BAT0 | awk -F\": *\" '/(percentage|state):/ { print $2 }'")
                         )
         (state (first battery-stats))
         (percent (second battery-stats)))
    (if (not (emptyp percent))
        (if (not (string= "discharging" state))
         (concat "~" percent *mode-line-sep*)
         (concat percent *mode-line-sep*))
      (concat ""))))

(defun get-volume ()
  (let* ((volume-stats (split-string
                        (run-shell-command "pulseaudio-ctl full-status" t)
                        " "))
         (percent (first volume-stats))
         (mutesign (if (string= (second volume-stats) "yes")
                    "x"
                    "")))
    (concat mutesign percent *mode-line-sep*)))

(defun get-mail ()
  (let ((emails (remove #\Newline (run-shell-command "notmuch count tag:unread" t))))
    (if (equal emails "0")
        ""
        (concat "^[^3*Mail: " emails "^]" *mode-line-sep*))))

(setf *screen-mode-line-left-side*
      (list " %d" *mode-line-sep* "%W"))

(setf *screen-mode-line-right-side*
      (list *mode-line-sep*
            '(:eval (get-mail))
            '(:eval (get-battery-info))
            '(:eval (get-volume))))

(defun parse-justified-mode-line ()
  (let* ((ml-left-side (mode-line-format-elt
                     *screen-mode-line-left-side*))
         (ml-right-side (mode-line-format-elt
                      *screen-mode-line-right-side*))
         (ml-length (+ (length ml-left-side)
                       (length ml-right-side)))
         (pixels-per-char (text-line-width
                          (screen-font (current-screen)) "M"))
         (num-head-columns (/ (head-width (current-head)) pixels-per-char))
         (padding (- (+ num-head-columns 6) ml-length)))

  (format nil "~a~va~a"
          ml-left-side padding " " ml-right-side)))

(setf *screen-mode-line-format*
      (list '(:eval (parse-justified-mode-line))))

(setf *mode-line-pad-x* 0)
(setf *time-modeline-string*
      "%a %b %e %R")

(setf *window-format*
      "%n:%15c")

(loop for head in (screen-heads (current-screen)) do
      (toggle-mode-line (current-screen) head))
