(in-package :stumpwm)

(defmacro make-program-binding (program-name window-class key &optional alias)
  "Add keybindings for a program to *root-map*. window-class is the
windows-class key is the key for the keybinding. C-key will open a new instance
of program-name, key will run-or-raise program-name, and (string-upcase key)
will run-or-pull program-name. Sticking to the theme of capitialized commands
move windows."

  (if (not alias)
      (setf alias program-name))

  `(progn
     (defcommand ,(intern (format nil "run-new-~a" alias)) () ()
       (run-shell-command ,program-name))

     (defcommand ,(intern (format nil "run-or-raise-~a" alias)) () ()
       (run-or-raise ,program-name '(:class ,window-class)))

     (defcommand ,(intern (format nil "run-or-pull-~a" alias)) () ()
       (run-or-pull ,program-name '(:class ,window-class)))

     (define-key *root-map* (kbd ,(format nil "~a" key))
       ,(format nil "run-or-raise-~a" alias))

     (define-key *root-map* (kbd ,(format nil "~a" (string-upcase key)))
       ,(format nil "run-or-pull-~a" alias))

     (define-key *root-map* (kbd ,(format nil "C-~a" key))
       ,(format nil "run-new-~a" alias))))

(make-program-binding "luakit" "Luakit" "b" "browser")
(make-program-binding "chromium" "Chromium" "M-b" "alt-browser")
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
    (run-shell-command (format nil "~a \'~a\'" *browser* search-term))))

(defcommand app-search-scholar () ()
  (let ((search-term (read-one-line (current-screen)
                                    "Search scholar: ")))
    (run-shell-command (format nil "~a \'!scholar ~a\'" *browser* search-term))))

(defcommand toggle-stumpwm-repl () ()
  (run-shell-command "emacsclient -c -e \"(open-stumpwm-repl)\""))
(defcommand app-open-email () ()
  "*email-url* defined in secrets.lisp"
  (run-shell-command (concat *alt-browser* " " *email-url*)))

(defcommand app-open-teams () ()
  "*email-url* defined in secrets.lisp"
  (run-shell-command (concat *alt-browser* " teams.webex.com")))


(defparameter *app-map*
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "f") (concat "exec " *terminal* " -e ranger"))
    (define-key m (kbd "d") "app-search-duck")
    (define-key m (kbd "s") "app-search-scholar")
    (define-key m (kbd ",") "toggle-stumpwm-repl")
    (define-key m (kbd "m") "app-open-email")
    (define-key m (kbd "t") "app-open-teams")
    m))

(define-key *root-map* (kbd "p") *pass-map*)
(define-key *root-map* (kbd "a") *app-map*)
