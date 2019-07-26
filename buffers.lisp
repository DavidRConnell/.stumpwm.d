(in-package :stumpwm)

;; Frame Splitting and moving
(defparameter *window-map*
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "s") "vsplit")
    (define-key m (kbd "v") "hsplit")
    (define-key m (kbd "c") "remove")
    (define-key m (kbd "o") "only")
    (define-key m (kbd "l") "windowlist")
    (define-key m (kbd "0") "pull-window-by-number 0")
    (define-key m (kbd "1") "pull-window-by-number 1")
    (define-key m (kbd "2") "pull-window-by-number 2")
    (define-key m (kbd "3") "pull-window-by-number 3")
    (define-key m (kbd "4") "pull-window-by-number 4")
    (define-key m (kbd "5") "pull-window-by-number 5")
    (define-key m (kbd "6") "pull-window-by-number 6")
    (define-key m (kbd "7") "pull-window-by-number 7")
    (define-key m (kbd "8") "pull-window-by-number 8")
    (define-key m (kbd "9") "pull-window-by-number 9")
    m))

(define-key *root-map* (kbd "w") *window-map*)
(define-key *root-map* (kbd "j") "move-focus down")
(define-key *root-map* (kbd "k") "move-focus up")
(define-key *root-map* (kbd "l") "move-focus right")
(define-key *root-map* (kbd "h") "move-focus left")
(define-key *root-map* (kbd "C-j") "move-focus down")
(define-key *root-map* (kbd "C-k") "move-focus up")
(define-key *root-map* (kbd "C-l") "move-focus right")
(define-key *root-map* (kbd "C-h") "move-focus left")
(define-key *root-map* (kbd "J") "move-window down")
(define-key *root-map* (kbd "K") "move-window up")
(define-key *root-map* (kbd "L") "move-window right")
(define-key *root-map* (kbd "H") "move-window left")
(define-key *root-map* (kbd "o") "other")

;; Buffers
(define-key *root-map* (kbd "[") "pull-hidden-previous")
(define-key *root-map* (kbd "]") "pull-hidden-next")

;; Groups
(defparameter *group-map*
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "n") "gnew")
    (define-key m (kbd "q") "gkill")
    (define-key m (kbd "l") "grouplist")
    m))

(define-key *root-map* (kbd "g") *group-map*)
(define-key *root-map* (kbd "{") "gprev")
(define-key *root-map* (kbd "}") "gnext")
(define-key *root-map* (kbd "0") "gselect 0")
(define-key *root-map* (kbd "1") "gselect 1")
(define-key *root-map* (kbd "2") "gselect 2")
(define-key *root-map* (kbd "3") "gselect 3")
(define-key *root-map* (kbd "4") "gselect 4")
(define-key *root-map* (kbd "5") "gselect 5")
(define-key *root-map* (kbd "6") "gselect 6")
(define-key *root-map* (kbd "7") "gselect 7")
(define-key *root-map* (kbd "8") "gselect 8")
(define-key *root-map* (kbd "9") "gselect 9")
