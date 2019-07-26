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

(load-module :battery-portable)
(set-font "-*-dejavu sans mono-bold-r-*-*-14-*-*-*-*-*-*-*")

(setf *screen-mode-line-format*
      (list '(:eval (battery-format)) " " '(:eval (time-format "%H:%M")) " | %W"))

(setf *window-format* "%n %10c |")

;; Turn on the modeline
(toggle-mode-line (current-screen) (current-head))
