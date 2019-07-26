;;; Load package
(in-package :stumpwm)

(define-key *top-map* (kbd "XF86AudioLowerVolume") "exec pulseaudio-ctl down")
(define-key *top-map* (kbd "XF86AudioRaiseVolume") "exec pulseaudio-ctl up")
(define-key *top-map* (kbd "XF86AudioMute") "exec pulseaudio-ctl mute")
