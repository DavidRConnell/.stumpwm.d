;;; Setup pavol

;;; Load package
(in-package :stumpwm)

(define-key *top-map* (kbd "XF86AudioLowerVolume") "amixer-Front-1-")
(define-key *top-map* (kbd "XF86AudioRaiseVolume") "amixer-Front-1+")
(define-key *top-map* (kbd "XF86AudioMute") "amixer-Master-toggle pulse")
