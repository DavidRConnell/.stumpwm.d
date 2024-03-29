(in-package :stumpwm)
;; (load-module "winner-mode")

;; (add-hook *post-command-hook* (lambda (command)
;;                                 (when (member command winner-mode:*default-commands*)
;;                                   (winner-mode:dump-group-to-file))))

(setf *frame-number-map* "aoeuhtns")
(setf *mouse-focus-policy* :click)

;; Frame Splitting and moving
(defparameter *window-map*
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "s") "vsplit")
    (define-key m (kbd "v") "hsplit")
    (define-key m (kbd "c") "remove")
    (define-key m (kbd "o") "only")
    (define-key m (kbd "l") "windowlist")
    (define-key m (kbd "=") "balance-frames")
    (define-key m (kbd "<") "resize-direction left")
    (define-key m (kbd ">") "resize-direction right")
    (define-key m (kbd "+") "resize-direction up")
    (define-key m (kbd "-") "resize-direction down")
    (define-key m (kbd "|") "iresize")
    ;; (define-key m (kbd "u") "winner-undo")
    ;; (define-key m (kbd "C-r") "winner-redo")
    m))

(defparameter *goto-window-map*
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "a") "select-window-by-number 0")
    (define-key m (kbd "o") "select-window-by-number 1")
    (define-key m (kbd "e") "select-window-by-number 2")
    (define-key m (kbd "u") "select-window-by-number 3")
    (define-key m (kbd "h") "select-window-by-number 4")
    (define-key m (kbd "t") "select-window-by-number 5")
    (define-key m (kbd "n") "select-window-by-number 6")
    (define-key m (kbd "s") "select-window-by-number 7")
    m))

(define-key *root-map* (kbd "w") *window-map*)
(define-key *root-map* (kbd "C-w") "ace-window")
(define-key *root-map* (kbd "u") *goto-window-map*)
(define-key *root-map* (kbd "j") "move-focus down")
(define-key *root-map* (kbd "k") "move-focus up")
(define-key *root-map* (kbd "l") "move-focus right")
(define-key *root-map* (kbd "h") "move-focus left")
(define-key *root-map* (kbd "J") "move-window down")
(define-key *root-map* (kbd "K") "move-window up")
(define-key *root-map* (kbd "L") "move-window right")
(define-key *root-map* (kbd "H") "move-window left")
(define-key *root-map* (kbd "o") "other")
(define-key *root-map* (kbd "0") "select-window-by-number 0")
(define-key *root-map* (kbd "1") "select-window-by-number 1")
(define-key *root-map* (kbd "2") "select-window-by-number 2")
(define-key *root-map* (kbd "3") "select-window-by-number 3")
(define-key *root-map* (kbd "4") "select-window-by-number 4")
(define-key *root-map* (kbd "5") "select-window-by-number 5")
(define-key *root-map* (kbd "6") "select-window-by-number 6")
(define-key *root-map* (kbd "7") "select-window-by-number 7")
(define-key *root-map* (kbd "8") "select-window-by-number 8")
(define-key *root-map* (kbd "9") "select-window-by-number 9")
(define-key *root-map* (kbd "`") "select-window-by-number")
(define-key *root-map* (kbd "q") "delete")
(define-key *root-map* (kbd "Q") "killall")

(defcommand ace-window (&optional class program) ()
  "Go to other window if there's two windows; ask for window if there's more
Analagous to ace-window for emacs."
  (let* ((windows (filter-windows-by-class class))
	 (number-windows (length windows)))
    (cond ((and class
		(or (< number-windows 2)
		    (and (= number-windows 2)
			 (string= (window-class (current-window)) class))))
	   (stumpwm:run-or-raise program `(:class ,class)))
	  ((and (>= number-windows 2) class)
	   (stumpwm:windowlist "%n|%t" windows))
	  ((= number-windows 2)
	   (stumpwm:prev))
	  (t (stumpwm:windowlist "%n|%c: %t" windows)))))

(defun filter-windows-by-class (&optional class)
  (let ((all-windows (screen-windows (current-screen))))
    (if class
	(loop for w in all-windows
	      when (string= (window-class w) class)
		collect w)
	all-windows)))

(defcommand killall () ()
  "Run killall on current window's class"
  (let ((class (window-class (current-window))))
    (if (string= "Emacs" class)
	(run-shell-command "systemctl --user restart emacs.service")
	(run-shell-command (concat "killall " (string-downcase class))))))

;; Buffers
(define-key *root-map* (kbd "]") "next")
(define-key *root-map* (kbd "[") "prev")

;; Groups
(define-key *root-map* (kbd "M-n") "gnew")
(define-key *root-map* (kbd "M-q") "gkill")
(define-key *root-map* (kbd "M-o") "gother")
(define-key *root-map* (kbd "M-g") "grouplist")
(define-key *root-map* (kbd "M-0") "gselect 0")
(define-key *root-map* (kbd "M-1") "gselect 1")
(define-key *root-map* (kbd "M-2") "gselect 2")
(define-key *root-map* (kbd "M-3") "gselect 3")
(define-key *root-map* (kbd "M-4") "gselect 4")
(define-key *root-map* (kbd "M-5") "gselect 5")
(define-key *root-map* (kbd "M-6") "gselect 6")
(define-key *root-map* (kbd "M-7") "gselect 7")
(define-key *root-map* (kbd "M-8") "gselect 8")
(define-key *root-map* (kbd "M-9") "gselect 9")
(define-key *root-map* (kbd "C-h") "gprev")
(define-key *root-map* (kbd "C-l") "gnext")
(define-key *root-map* (kbd "M-h") "gprev-with-window")
(define-key *root-map* (kbd "M-l") "gnext-with-window")

(define-key *root-map* (kbd "z") "banish")
