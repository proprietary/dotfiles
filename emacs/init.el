;;; MELPA

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

(eval-when-compile
  (let ((default-directory (expand-file-name
			                (concat user-emacs-directory
				                    "third_party"))))
    (normal-top-level-add-subdirs-to-load-path))
  (require 'use-package))

(let ((default-directory (expand-file-name
                          (concat user-emacs-directory
                                  "site-lisp"))))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))


;;
;; MacOS
;; -----

(when (string-equal "darwin" system-type)
  (defun copy-from-osx ()
    (shell-command-to-string "pbpaste"))

  (defun paste-to-osx (text &optional push)
    (let ((process-connection-type nil))
      (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
        (process-send-string proc text)
        (process-send-eof proc))))

  (setq interprogram-cut-function 'paste-to-osx)
  (setq interprogram-paste-function 'copy-from-osx))

;; Make the shell PATH work in windowed GUI mode
(when (memq window-system '(mac ns x))
  (require 'exec-path-from-shell)
  (exec-path-from-shell-initialize))

;;
;; Git
;; ---
;;
(use-package magit :ensure t)

;;
;; Navigation
;; ----------
;;

;; 4 width spaces as indentation
(setq standard-indent 4)
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(setq c-basic-indent 4)

;; Customize word boundaries to treat '_' as part of words
(modify-syntax-entry ?_ "w")

;; Customize word boundaries to treat '-' as part of words
(modify-syntax-entry ?- "w")

;; Customize word boundaries to treat '/' as part of words
(modify-syntax-entry ?/ "w")
(put 'upcase-region 'disabled nil)

;; scroll smoothly one step at a time
(setq scroll-step 1
      scroll-conservatively 10000)

;; always highlight current line
(global-hl-line-mode)

;; Do not show the prompt: "Symbolic link to Git-controlled source file; follow link (y or n)"
(setq vc-follow-symlinks t)

;; prevent creation of junk tilde files
(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups"))))

;; Interactively Do Things -- fast buffer switch
(require 'ido)
(ido-mode 'buffers) ;; only use this line to turn off ido for file names!
(setq ido-ignore-buffers '("^ " "*Completions*" "*Shell Command Output*"
               "*Messages*" "Async Shell Command"))

;; refresh open files to latest version on disk automtically
;; useful when an external program modified a file; e.g., `clang-format`
(global-auto-revert-mode)

;; evil-mode

(use-package goto-chg :ensure t) ;; evil-mode dependency
(require 'evil)
(evil-mode 1)

(use-package paredit :ensure t)

(use-package projectile
  :ensure t
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map)))

;;
;; LSP
;; ---
;;

(use-package eglot :ensure t)

(use-package nix-mode :ensure t :mode "\\.nix\\'")

(use-package yaml-mode :ensure t)

(require 'skynet)

(use-package s :ensure t)
(use-package dash :ensure t)
(use-package editorconfig :ensure t)
(require 'copilot)
(add-hook 'prog-mode-hook 'copilot-mode)
(define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
(define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)

;;
;; Appearance
;; ----------
;;

;; theme
(load-theme 'tsdh-dark)

(use-package indent-bars
  :hook ((yaml-mode . indent-bars-mode)
         (python-mode . indent-bars-mode)))

(setq inhibit-start-screen t
      inhibit-startup-screen t)

(setq ring-bell-function nil)

(setq package-install-upgrade-built-in t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(bind-key eldoc faceup flymake jsonrpc org project soap-client tramp use-package use-package-ensure-system-package verilog-mode editorconfig projectile seq yaml-mode nix-mode magit eglot)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
