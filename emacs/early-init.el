;; font
(defparameter +zelcon/font-name+ "FiraCode Nerd Font Mono")
(set-face-attribute 'default nil :font +zelcon/font-name+ :height 120)

;; initial Emacs frame size
(setq 'initial-frame-alist
      '((top . 0) (left . 0)
        (width . 120) (height . 100)
        (font . +zelcon/font-name+)))
