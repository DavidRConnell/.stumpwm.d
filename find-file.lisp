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
                       (ls initial-path)
                       prompt
                       0
                       *find-file-map*))
         (abs-file-ns (concat (namestring initial-path) (car file-string)))
         (abs-file-path (pathname abs-file-ns)))
    (cond
      ((not file-string)
       (message "Abort."))
      ((directory-pathname-p abs-file-path)
       (find-file abs-file-ns))
      (t
       (run-shell-command (concat "xdg-open " abs-file-ns))))))

(defvar *find-file-map*
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "TAB") 'find-file-tab-complete)
    (define-key m (kbd "DEL") 'find-file-delete-or-back-directory)
    m))

(defcommand find-file-tab-complete () ()
      (stumpwm:next))

(defun find-file-delete-or-back-directory (input)
  (if (equal 0 (length input))
      (message "empty")
      (message "not empty")))

(defun string-substitute (new old string)
  "Replace substring OLD with substring NEW in STRING."
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
