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
       (ace-window ,window-class ,program-name))

     (defcommand ,(intern (format nil "run-or-pull-~a" alias)) () ()
       (run-or-pull ,program-name '(:class ,window-class)))

     (define-key *root-map* (kbd ,(format nil "~a" key))
       ,(format nil "run-or-raise-~a" alias))

     (define-key *root-map* (kbd ,(format nil "~a" (string-upcase key)))
       ,(format nil "run-or-pull-~a" alias))

     (define-key *root-map* (kbd ,(format nil "C-~a" key))
       ,(format nil "run-new-~a" alias))))

(load-config-file "browser-containers.lisp")
(make-program-binding "qutebrowser" "qutebrowser" "b" "browser")
(make-program-binding "firefox" "firefox" "M-b" "alt-browser")
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
        (run-shell-command (format nil "~a \'!article ~a\'" *browser* search-term)))))

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

(defcommand app-screenshot () ()
"Take a screenshot and save to ~/screenshots"
  ;; (run-shell-command "scrot -e 'mv $f ~/screenshots/' -s")
  (run-shell-command "flameshot gui"))

(defcommand open-radio () ()
 "Open spotify-player in a new terminal or open an existing one"
 (let ((name "*spotify*"))
   (if (filter-windows-by-name name)
       (select-window-by-name name)
       (run-shell-command
	(format nil "alacritty --title=\"~a\" --command=spotify_player" name)))))

(defun filter-windows-by-name (&optional name)
  (let ((all-windows (screen-windows (current-screen))))
    (if name
	(loop for w in all-windows
	      when (string= (window-name w) name)
	      collect w)
	all-windows)))


(defparameter *app-map*
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "b") "open-browser-container")
    (define-key m (kbd "r") "open-radio")
    (define-key m (kbd "d") "app-search-duck")
    (define-key m (kbd "D") "app-search-doi")
    (define-key m (kbd "w") "app-search-wiki")
    (define-key m (kbd "W") "app-search-wiktionary")
    (define-key m (kbd "g") "app-search-gene")
    (define-key m (kbd "s") "app-search-scholar")
    (define-key m (kbd "m") "app-open-email")
    (define-key m (kbd "x") "app-screenshot")
    m))

(define-key *root-map* (kbd "p") *pass-map*)
(define-key *root-map* (kbd "a") *app-map*)
