(in-package :stumpwm)
(use-package :cl-ppcre)

(defcommand find-file (initial-path) ((:string "Starting path: "))
    "Find-file starting search at INITIAL-PATH.
If selected menu option is a directory start searching in that directory,
if selected menu optino is a file open with xdg-open.
A selection of nil (through C-g) aborts."
  (let* ((formatted-initial-path
           (string-substitute "~/" (namestring (user-homedir-pathname)) (namestring initial-path)))
         (prompt (format nil "Find-file: ~a^B" formatted-initial-path))
           (file-string (select-from-menu
                  (current-screen)
                            (mapcar #'list (ls initial-path))
                            prompt))
           (abs-file-ns (concat (namestring initial-path) (car file-string)))
           (abs-file-path (pathname abs-file-ns)))
    (cond
      ((not file-string)
       (message "Abort."))
      ((directory-pathname-p abs-file-path)
       (find-file abs-file-ns))
      (t
       (run-shell-command (concat "xdg-open " abs-file-ns))))))

(defun string-substitute (new old string)
  (if (string-match string old)
      (let ((sub-strings (split old string)))
        (concat (first sub-strings) new (second sub-strings)))
      string))

(defun ls (directory)
  (let ((files (list-directory directory))
        (length-dir-name (length (namestring directory))))
    (mapcar
     (lambda (path) (subseq (namestring path) length-dir-name))
     files)))
