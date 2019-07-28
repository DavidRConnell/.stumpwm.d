(in-package :stumpwm)

(defmacro make-program-binding (program-name window-class key &optional alias)
  "Add keybindings for a program to *root-map*.
window-class is the windows-class
key is the key for the keybinding.
C-key will open a new instance of program-name,
key will run-or-pull program-name."

  (if (not alias)
      (setf alias program-name))

  `(progn
     (defcommand ,(intern (format nil "~a" alias)) () ()
       (run-shell-command ,program-name))

     (defcommand ,(intern (format nil "run-or-pull-~a" alias)) () ()
       (run-or-pull ,program-name '(:class ,window-class)))

     (define-key *root-map* (kbd ,(format nil "~a" key))
       ,(format nil "run-or-pull-~a" alias))

     (define-key *root-map* (kbd ,(format nil "C-~a" key))
       ,(format nil "~a" alias))))

(make-program-binding "vimb" "Vimb" "b")
(make-program-binding "alacritty" "Alacritty" "t" "terminal")
(make-program-binding "emacsclient -c -a ''" "Emacs" "e" "emacs")

;;; System Command Keymap
(load-module "pass")
(defparameter *pass-map*
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "c") "pass-copy-menu")
    (define-key m (kbd "g") "pass-generate")
    m))

(defcommand app-search-duck () ()
  (let ((search-term (read-one-line (current-screen)
                                    "Search duck: ")))
    (run-shell-command (format nil "vimb \"~a\"" search-term))))

(defcommand app-search-scholar () ()
  (let ((search-term (read-one-line (current-screen)
                                    "Search scholar: ")))
    (run-shell-command (format nil "vimb \"sch ~a\"" search-term))))

(defcommand toggle-stumpwm-repl () ()
  (run-shell-command "emacsclient -c -e \"(open-stumpwm-repl)\""))

(defparameter *app-map*
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "f") "exec alacritty -e vifm")
    (define-key m (kbd "d") "app-search-duck")
    (define-key m (kbd "s") "app-search-scholar")
    (define-key m (kbd ",") "toggle-stumpwm-repl")
    m))

(define-key *root-map* (kbd "p") *pass-map*)
(define-key *root-map* (kbd "a") *app-map*)
