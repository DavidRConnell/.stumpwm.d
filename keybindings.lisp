(in-package :stumpwm)

;; Double check prefix-key is set correctly
(set-prefix-key (kbd "C-t"))

;; Frame Splitting and moving
(defparameter *frame-map*
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "s") "vsplit")
    (define-key m (kbd "v") "hsplit")
    (define-key m (kbd "c") "remove")
    (define-key m (kbd "o") "only")
    m))

(define-key *root-map* (kbd "w") *frame-map*)
(define-key *root-map* (kbd "j") "move-focus down")
(define-key *root-map* (kbd "k") "move-focus up")
(define-key *root-map* (kbd "l") "move-focus right")
(define-key *root-map* (kbd "h") "move-focus left")

;; Buffers
(define-key *root-map* (kbd "[") "pull-hidden-previous")
(define-key *root-map* (kbd "]") "pull-hidden-next")

(defmacro make-program-binding (program-name window-class &optional alias)
  "Create run-or-raise and run-or-pull commands for program-name
window-class is the windows-class
Also add keybinding to the commands. 
C-keybinding r calls run-or-raise
C-keybinding p calls run-or-pull
C-keybinding n creates a new instance of the program"
  (if (not alias)
      (setf alias program-name))
  `(progn
     (defvar ,(intern (format nil "*~a-map*" alias)) nil)

     (defcommand ,(intern (format nil "~a" alias)) () () (run-shell-command ,program-name))
     
     (defcommand ,(intern (format nil "run-or-raise-~a" alias)) () ()
		 (run-or-raise ,program-name '(:class ,window-class)))
     
     (defcommand ,(intern (format nil "run-or-pull-~a" alias)) () ()
		 (run-or-pull ,program-name '(:class ,window-class)))
     
     (fill-keymap ,(intern (format nil "*~a-map*" alias))
		  (kbd "p") ,(format nil "run-or-pull-~a" alias)
		  (kbd "r") ,(format nil "run-or-raise-~a" alias)
		  (kbd "n") ,(format nil "~a" alias))))

(make-program-binding "vimb" "Vimb")

(make-program-binding "deepin-terminal" "Terminal" "terminal")

(make-program-binding "emacsclient -c -a ''" "Emacs" "emacs")

(define-key *root-map* (kbd "C-b") "exec vimb")
(define-key *root-map* (kbd "C-t") "exec deepin-terminal")
(define-key *root-map* (kbd "C-e") "exec emacsclient -c -a 'emacs --daemon'")
(define-key *root-map* (kbd "e") |*emacs-map*|)
(define-key *root-map* (kbd "b") |*vimb-map*|)
(define-key *root-map* (kbd "t") |*terminal-map*|)

(defun focus-current-frame-on-other-head (group)
  "Focus first frame on the next head."
  (let* ((remaining-heads (cdr (member (group-current-head group) (screen-heads (current-screen)))))
	 (other-head (if (null remaining-heads)
			 (first (screen-heads (current-screen)))
			 (car remaining-heads))))
    (focus-frame group (first (remove-if-not (lambda (frame)
					       (eql (frame-head group frame)
						    other-head))
					     (group-frames group))))))

;; Setup bindings for less common aplications which would be opened then closed
(defcommand screenshot () ()
  "Do we wanna Scrot? Yeah! We wanna Scrot!"
  (run-shell-command "cd /home/thomas/Pictures/screenshots/; scrot"))

(defcommand screenshot-name () ()
	    "Do we wanna Scrot? Yeah! We wanna Scrot!"
	    (run-shell-command (concat "cd /home/thomas/Pictures/screenshots/; scrot temp.png") t)
	    (let ((filename (read-one-line (current-screen) "Filename:")))
	      (run-shell-command (concat "cd /home/thomas/Pictures/screenshots/; mv ./temp.png ./" filename ".png"))))

(defcommand volume-control () ()
	    "Start volume control"
	    (run-or-raise "pavucontrol" '(:class "Pavucontrol")))

;;; System Command Keymap
(defparameter *screenshot-map*
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "f") "screenshot")
    (define-key m (kbd "n") "screenshot-name")
    m))

(defparameter *pass-map*
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "c") "pass-copy")
    (define-key m (kbd "m") "pass-copy-menu")
	(define-key m (kbd "g") "pass-generate")
    m) )

(defparameter *system-map*
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "s") *screenshot-map*)
    (define-key m (kbd "p") "end-session")
    m))

(define-key *root-map* (kbd "s") *system-map*)
(define-key *root-map* (kbd "p") *pass-map*)
