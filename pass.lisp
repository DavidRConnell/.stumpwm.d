(defvar *password-store*
  (if (uiop:getenv "PASSWORD_STORE_DIR")
      (uiop:getenv "PASSWORD_STORE_DIR")
      (merge-pathnames #p".local/share/password-store/"
		       (user-homedir-pathname)))
  "Location to search for names in the password store, according to the XDG Base
Directory Specification. Tries $PASSWORD_STORE_DIR then $HOME/.local/share/password-store/.")

(defvar *pass-last-entry* "")

(defun pass-last-entry-index-rec (lst idx)
  (if (equal (length lst) idx)
      0
      (if (equal *pass-last-entry* (nth idx lst))
	  idx
	  (pass-last-entry-index-rec lst (+ idx 1)))))

(defun pass-last-entry-index ()
  (if (uiop:emptyp *pass-last-entry*)
      0
      (pass-last-entry-index-rec (pass-entries) 0)))


(defun pass-entries ()
  (let ((home-ns-len (length (namestring *password-store*))))
    (mapcar (lambda (entry)
	      (let ((entry-ns (namestring entry)))
		(subseq entry-ns home-ns-len (- (length entry-ns) 4))))
	    (directory (make-pathname :directory
				      `(,@(pathname-directory *password-store*)
					  :wild-inferiors)
				      :name :wild
				      :type "gpg")))))

(defun pass-select-from-menu (message)
  (let* ((selection (pass-last-entry-index))
	(entry (stumpwm:select-from-menu
		(stumpwm:current-screen)
		(pass-entries)
		message
		selection)))
    (if entry
	(progn
	  (setf *pass-last-entry* (car entry))
	  (car entry))
	nil)))

(defun pass-run-command (subcmd entry &optional collect-output-p extra-cmds)
  (if entry
      (stumpwm:run-shell-command
       (format nil "PASSWORD_STORE_DIR=~a pass ~a ~a ~a"
	       *password-store* subcmd entry extra-cmds) collect-output-p)))

(defun pass-list-keys (entry)
  (split-string (pass-run-command "show" entry t
				  (format nil "| sed -n 's/\\s*\\(.*\\):\\s\\+.*$/\\1/p'"))
		(format nil "~C" #\Newline)))

(defun pass-get-value (entry key)
  (pass-run-command "show" entry nil
		    (format nil
			    "| sed -n 's/\\s*~a:\\s\\+\\(.*$\\)/\\1/p' | tr -d '\\n' | xclip -sel clip"
			    key)))

(stumpwm:defcommand pass-copy () ()
  "Select a password entry from a menu and copy the password into the clipboard."
  (pass-run-command "-c" (pass-select-from-menu "Copy password to clipboard: ")))

(stumpwm:defcommand pass-copy-field () ()
  "Select a password entry from a menu, then select a field to copy into the clipboard."
  (let* ((entry (pass-select-from-menu "Copy field to clipboard: ")))
    (pass-get-value entry
		    (car (stumpwm:select-from-menu
			  (stumpwm:current-screen)
			  (pass-list-keys entry)
			  "Select field to copy: ")))))

(stumpwm:defcommand pass-copy-user () ()
  "Select a password entry from a menu and copy the password into the clipboard."
  (pass-get-value (pass-select-from-menu "Copy username to clipboard: ") "username"))

(stumpwm:defcommand pass-generate () ()
 "Generate a password and put it into the clipboard"
 (let ((entry-name (stumpwm:read-one-line (stumpwm:current-screen)
					  "entry name: ")))
   (if entry-name
       (pass-run-command "generate -c" entry-name))))

(defun pass-yes-or-no-p (prompt retries)
  (stumpwm:message prompt)
  (if (equal retries 0)
      nil
      (let ((response (stumpwm:read-one-char (stumpwm:current-screen))))
	(cond ((equal response #\y) t)
	      ((equal response #\n) nil)
	      (t (pass-yes-or-no-p
		  (concat "Response must be y or n, " prompt)
		  (- retries 1)))))))

(stumpwm:defcommand pass-update () ()
  "Generate a new password for an existing entry"
  (let ((entry (pass-select-from-menu "Update password: ")))
    (if (pass-yes-or-no-p (concat "Generate new password for " entry "?") 3)
	(progn
	  (message "Updating")
	  (pass-run-command "generate -c -i -f" entry))
	(stumpwm:message "Aborting"))))

(stumpwm:defcommand pass-otp () ()
  "Get a one time pass. Requires pass-otp extension."
  (pass-run-command "otp -c" (pass-select-from-menu
			      "Copy one time pass to clipboard: ")))
