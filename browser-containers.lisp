;; Why doesn't stumpwm know about these?
(defvar *XDG_DATA_HOME* (merge-pathnames #p".local/share" (user-homedir-pathname)))
(defvar *XDG_CONFIG_HOME* (merge-pathnames #p".config" (user-homedir-pathname)))

(defvar *container_script* (merge-pathnames #p"bin/browser" (user-homedir-pathname)))

(defun containers ()
  (stumpwm:split-string (stumpwm:run-shell-command
			 (format nil "XDG_DATA_HOME=~a ~a --list"
				 *XDG_DATA_HOME*
				 *container_script*)
			 t)
			(format nil "~C~a" #\Newline " ")))

(defcommand open-browser-container () ()
  (let ((entry (stumpwm:select-from-menu
		(stumpwm:current-screen)
		(mapcar 'list (containers))
		"entry: ")))
    (stumpwm:run-shell-command (format nil "XDG_CONFIG_HOME=~a XDG_DATA_HOME=~a ~a ~a"
				       *XDG_CONFIG_HOME*
				       *XDG_DATA_HOME*
				       *container_script*
				       (car entry)))))
