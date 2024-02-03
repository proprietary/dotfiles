;;
;; Initial Setup
;; -------------
;;

;;; MELPA

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
             (cons "nongnu" (format "http%s://elpa.nongnu.org/nongnu/"
                                    (if (gnutls-available-p) "s" ""))))
(package-initialize)

;; Load vendored packages

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

;; Common Lisp extensions to Emacs Lisp
(require 'cl)

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
;; evil-mode
;; ---------
;;
(use-package goto-chg :ensure t) ;; evil-mode dependency
(require 'evil)
(evil-mode 1)
(evil-set-initial-state 'messages-buffer-mode 'emacs)
(evil-set-initial-state 'help-mode 'emacs)
(evil-set-initial-state 'package-menu-mode 'emacs)
(evil-set-initial-state 'magit-mode 'emacs)
(evil-set-initial-state 'bs-mode 'emacs)
(evil-set-initial-state 'ibuffer-mode 'emacs)
(evil-set-initial-state 'dired-mode 'emacs)
(evil-set-initial-state 'rg-mode 'emacs)
(evil-set-initial-state 'xref--xref-buffer-mode 'emacs)
(add-hook 'xref-backend-functions #'(lambda (&rest _) (evil-emacs-state)))
(evil-set-initial-state 'compilation-mode 'emacs)
(evil-set-initial-state 'shell-mode 'emacs)
(evil-set-initial-state 'term-mode 'emacs)
(evil-set-initial-state 'vterm-mode 'emacs)
(evil-set-initial-state 'debugger-mode 'emacs)
(evil-set-initial-state 'special-mode 'emacs)
(evil-define-key 'normal 'global (kbd "SPC i") 'imenu)


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

;; Interactively Do Things -- fast buffer switch
(require 'ido)
(ido-mode 'buffers) ;; only use this line to turn off ido for file names!
(setq ido-ignore-buffers '("^ " "*Completions*" "*Shell Command Output*"
               "*Messages*" "Async Shell Command"))

;; completion
(global-company-mode)
(setq company-idle-delay 0.1)

;;
;; Annoying Defaults
;; -----------------
;;

(setq ring-bell-function nil)

(setq inhibit-start-screen t
      inhibit-startup-screen t)

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

(use-package paredit :ensure t)

(use-package projectile
  :ensure t
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map)))

;;
;; Language Support
;; ----------------
;;

;; tree-sitter
(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
        (c "https://github.com/tree-sitter/tree-sitter-c")
        (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
        (java "https://github.com/tree-sitter/tree-sitter-java")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

(defun zelcon/install-tree-sitter-langs ()
  "Install all tree-sitter languages. Typically you only need to run
this once."
  (interactive)
  (mapc #'treesit-install-language-grammar
        (mapcar #'car treesit-language-source-alist)))

(setq major-mode-remap-alist
      '((yaml-mode . yaml-ts-mode)
        (bash-mode . bash-ts-mode)
        (json-mode . json-ts-mode)
        (python-mode . python-ts-mode)
        (java-mode . java-ts-mode)
        (c-mode . c-ts-mode)
        (c++-mode . cpp-ts-mode)))

(use-package eglot
  :ensure t
  :hook '((python-ts-mode . eglot-ensure)
          (python-mode . eglot-ensure)
          (scala-mode . eglot-ensure)
          (rust-mode . eglot-ensure)
          (c-ts-mode . eglot-ensure)
          (c++-ts-mode . eglot-ensure)
          (java-mode . eglot-ensure)
          (java-ts-mode . eglot-ensure)
          (js-mode . eglot-ensure)
          (typescript-mode . eglot-ensure)
          (lua-mode . eglot-ensure)
          (haskell-mode . eglot-ensure)
          (lisp-mode . eglot-ensure)
          (go-ts-mode . eglot-ensure))
  :bind (:map eglot-mode-map
              ("C-c C-a" . eglot-code-actions)))

(use-package nix-mode :ensure t :mode "\\.nix\\'")

(use-package yaml-mode :ensure t)

;; Github Copilot
(use-package s :ensure t)
(use-package dash :ensure t)
(use-package editorconfig :ensure t)
(require 'copilot)
(add-hook 'prog-mode-hook 'copilot-mode)
(define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
(define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)
(define-key copilot-completion-map (kbd "C-TAB") 'copilot-accept-completion-by-word)
(define-key copilot-completion-map (kbd "C-<tab>") 'copilot-accept-completion-by-word)
(evil-define-key 'insert 'global (kbd "<tab>") #'(lambda ()
                                                   (interactive)
                                                   (if (and (bound-and-true-p copilot-mode)
                                                            (copilot-completion-active-p))
                                                       (copilot-accept-completion)
                                                     (evil-insert 1))))


(use-package spinner :ensure t)

(use-package ellama :ensure t
  :init
  (require 'llm-ollama)
  (setopt ellama-provider
          (make-llm-ollama
           :chat-model
           "deepseek-coder:6.7b-instruct"
           )))

;; completions
(setq completions-format 'one-column)
(setq completions-header-format nil)
(setq completions-max-height 20)
(setq completion-auto-select nil)
(define-key minibuffer-local-completion-map (kbd "C-n") 'minibuffer-next-line-completion)
(define-key minibuffer-local-completion-map (kbd "C-p") 'minibuffer-previous-line-completion)
(define-key global-map (kbd "C-c b") 'switch-to-minibuffer)

(fido-mode 1)
(fido-vertical-mode 1)
(setq fido-vertical-mode-show-count t)

;;
;; Appearance
;; ----------
;;

;; theme
(use-package solarized-theme
  :ensure t
  :init
  (load-theme 'solarized-dark t))

(use-package challenger-deep-theme
  :disabled
  :ensure t
  :config
  (load-theme 'challenger-deep t))

;; font
(set-face-attribute 'default nil :font "FiraCode Nerd Font Mono" :height 120)

;; line numbers
(global-display-line-numbers-mode)
(setq display-line-numbers-type 'relative)

(require 'indent-bars)
(eval-after-load 'indent-bars
  (progn
    (setq indent-bars-treesit-support t)
    (setq indent-bars-no-descend-string t)
    (setq indent-bars-treesit-ignore-blank-lines-types '("module"))
    (setq indent-bars-treesit-wrap '((python argument_list parameters ; for python, as an example
                                             list list_comprehension
                                             dictionary dictionary_comprehension
                                             parenthesized_expression subscript)))
    (add-hook 'python-base-mode 'indent-bars-mode)
    (add-hook 'yaml-mode 'indent-bars-mode)))


(setq package-install-upgrade-built-in t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(bind-key challenger-deep-theme company doom-themes editorconfig
              eglot eldoc ellama faceup flymake jsonrpc magit nix-mode
              org project projectile seq soap-client solarized-theme
              spinner tramp use-package
              use-package-ensure-system-package verilog-mode yaml-mode))
 '(warning-suppress-log-types
   '(((copilot copilot-no-mode-indent))
     ((copilot copilot-no-mode-indent))
     ((copilot copilot-no-mode-indent)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
