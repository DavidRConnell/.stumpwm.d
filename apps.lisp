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

(make-program-binding "qutebrowser" "qutebrowser" "b" "browser")
(make-program-binding "chromium" "Chromium" "M-b" "alt-browser")
(make-program-binding "alacritty" "Alacritty" "t" "terminal")
(make-program-binding "emacsclient -c -a ''" "Emacs" "e" "emacs")
(make-program-binding "zathura" "Zathura" "f" "zathura")

;;; System Command Keymap
(load-config-file "pass.lisp")
(defparameter *pass-map*
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "p") "pass-copy-menu")
    (define-key m (kbd "u") "pass-copy-user-menu")
    (define-key m (kbd "g") "pass-generate")
    m))

(defcommand app-search-duck () ()
  (let ((search-term (read-one-line (current-screen)
                                    "Search duck: ")))
    (if search-term
        (run-shell-command (format nil "~a \'~a\'" *browser* search-term)))))

(defcommand app-search-scholar () ()
  (let ((search-term (read-one-line (current-screen)
                                    "Search scholar: ")))
    (if search-term
        (run-shell-command (format nil "~a \'!scholar ~a\'" *browser* search-term)))))

(defcommand app-search-wiki () ()
  (let ((search-term (read-one-line (current-screen)
                                    "Search wiki: ")))
    (if search-term
        (run-shell-command (format nil "~a \'!wiki ~a\'" *browser* search-term)))))

(defcommand app-search-wiktionary () ()
  (let ((search-term (read-one-line (current-screen)
                                    "Search wiktionary: ")))
    (if search-term
        (run-shell-command (format nil "~a \'!wiktionary ~a\'" *browser* search-term)))))

(defcommand app-search-gene () ()
  (let ((search-term (read-one-line (current-screen)
                                    "Search for gene: ")))
    (if search-term
        (run-shell-command (format nil
                                   "~a \'www.ncbi.nlm.nih.gov/gene/~a\'"
                                   *browser*
                                   search-term)))))

(defcommand app-search-doi () ()
  (let ((search-term (read-one-line (current-screen) "Search by doi: ")))
    (if search-term
        (run-shell-command (format nil "~a \'doi.org/~a\'" *browser* search-term)))))

(defcommand app-open-email () ()
  "*email-url* defined in profile.lisp"
  (run-shell-command (concat *browser* " " *email-url*)))

(defcommand app-open-teams () ()
  "*email-url* defined in profile.lisp"
  (run-shell-command (concat *browser* " teams.webex.com")))

(defcommand app-org-capture () ()
  "Run org-capture"
  (run-shell-command "org-capture -k i"))

(defcommand app-quick-capture () ()
  "Use stumpwm input menu to capture a message to inbox.org."
  (let ((note (read-one-line (current-screen) "Message: "))
        (note-file "~/notes/zettle/inbox.org"))
    (run-shell-command (format nil "echo \'* ~a\' >> ~a" note note-file))))

(defparameter *app-map*
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "f") (concat "exec " *terminal* " -e ranger"))
    (define-key m (kbd "d") "app-search-duck")
    (define-key m (kbd "D") "app-search-doi")
    (define-key m (kbd "w") "app-search-wiki")
    (define-key m (kbd "W") "app-search-wiktionary")
    (define-key m (kbd "g") "app-search-gene")
    (define-key m (kbd "s") "app-search-scholar")
    (define-key m (kbd "m") "app-open-email")
    (define-key m (kbd "t") "app-open-teams")
    (define-key m (kbd "x") "app-org-capture")
    (define-key m (kbd "q") "app-quick-capture")
    m))

(define-key *root-map* (kbd "p") *pass-map*)
(define-key *root-map* (kbd "a") *app-map*)
(define-key *root-map* (kbd "C-f") "find-file /home/voidee/")
