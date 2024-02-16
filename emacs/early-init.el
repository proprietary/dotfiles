;; font
(defconst +zelcon/font-name+ "FiraCode Nerd Font")
(set-face-attribute 'default nil :family +zelcon/font-name+ :height 120)

;; initial Emacs frame size
(add-to-list 'initial-frame-alist '(width . 120))
(add-to-list 'initial-frame-alist '(height . 100))
