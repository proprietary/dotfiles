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
(require 'cl-lib)

;; set customizations file
;; but don't load this until the end
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))


;;
;; System Copy & Paste
;; --------------------

(cl-case system-type
  (gnu/linux
   (progn
     (if (string-equal (getenv "WAYLAND_DISPLAY") "wayland")
         (progn
           (setq interprogram-cut-function
                 (lambda (text &optional push)
                   (let* ((process-connection-type nil)
                          (proc (start-process "wl-copy" "*Messages*" "wl-copy")))
                     (process-send-string proc text)
                     (process-send-eof proc))))
           (setq interprogram-paste-function
                 (lambda (text &optional push)
                   (shell-command-to-string "wl-paste -n"))))
       (progn
         (setq interprogram-cut-function
               (lambda (text &optional push)
                 (let* ((process-connection-type nil)
                        (proc (start-process "xsel" "*Messages*" "xsel" "-i" "-b")))
                   (process-send-string proc text)
                   (process-send-eof proc))))
         (setq interprogram-paste-function
               (lambda ()
                 (shell-command-to-string "xsel -o -b")))))))
  (darwin
   (progn
     (defun zelcon/copy-from-osx ()
       (shell-command-to-string "pbpaste"))

     (defun zelcon/paste-to-osx (text &optional push)
       (let ((process-connection-type nil))
         (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
           (process-send-string proc text)
           (process-send-eof proc))))

     (setq interprogram-cut-function 'zelcon/paste-to-osx)
     (setq interprogram-paste-function 'zelcon/copy-from-osx)))

  (windows-nt
   (progn
     (setq interprogram-cut-function
           (lambda (text &optional push)
             (let* ((process-connection-type nil)
                    (proc (start-process "clip" "*Messages*" "clip")))
               (process-send-string proc text)
               (process-send-eof proc))))
     (setq interprogram-paste-function
           (lambda ()
             (shell-command-to-string "powershell.exe Get-Clipboard"))))))

;;
;; MacOS
;; -----

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

;; set some modes to use emacs mode by default
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
(evil-set-initial-state 'treemacs-mode 'emacs)
(evil-set-initial-state 'messages-buffer-mode 'emacs)
(evil-define-key 'normal 'global (kbd "SPC i") 'imenu)
(add-hook 'special-mode-hook 'evil-emacs-state)


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
(use-package company
  :ensure t
  :config
  (global-company-mode)
  (setq company-idle-delay 0.1))


(setq ring-bell-function nil)

(setq inhibit-start-screen t
      inhibit-startup-screen t)

;; Do not show the prompt: "Symbolic link to Git-controlled source file; follow link (y or n)"
(setq vc-follow-symlinks t)

;; prevent creation of junk tilde files
(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups")))
      backup-by-copying t
      version-control t
      delete-old-versions t
      kept-old-versions 100
      create-lockfiles nil)
;; ensure that auto-save files end up in the right place
(append auto-save-file-name-transforms
        `((".*" ,(concat user-emacs-directory "backups") t)))

;; Interactively Do Things -- fast buffer switch
(require 'ido)
(ido-mode 'buffers) ;; only use this line to turn off ido for file names!
(setq ido-ignore-buffers '("^ " "*Completions*" "*Shell Command Output*"
               "*Messages*" "Async Shell Command"))

;; refresh open files to latest version on disk automtically
;; useful when an external program modified a file; e.g., `clang-format`
(global-auto-revert-mode)

(use-package paredit :ensure t :disabled)

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
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")
        (starlark "https://github.com/amaanq/tree-sitter-starlark")
        (lua "https://github.com/tree-sitter-grammars/tree-sitter-lua")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript")
        (swift "https://github.com/alex-pinkus/tree-sitter-swift")
        (toml "https://github.com/ikatyang/tree-sitter-toml")
        (latex "https://github.com/latex-lsp/tree-sitter-latex")
        (rust "https://github.com/tree-sitter/tree-sitter-rust")
        (ruby "https://github.com/tree-sitter/tree-sitter-ruby")
        (r "https://github.com/r-lib/tree-sitter-r")
        (make "https://github.com/alemuller/tree-sitter-make")
        (julia "https://github.com/tree-sitter/tree-sitter-julia")
        (rst "https://github.com/stsewd/tree-sitter-rst")
        (nix "https://github.com/nix-community/tree-sitter-nix")))

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
        (c++-mode . cpp-ts-mode)
        (rust-mode . rust-ts-mode)
        (ruby-mode . ruby-ts-mode)
        (r-mode . r-ts-mode)
        (lua-mode . lua-ts-mode)
        (julia-mode . julia-ts-mode)
        (lua-mode . lua-ts-mode)
        (js-mode . javascript-ts-mode)
        (typescript-mode . typescript-ts-mode)
        (go-mode . go-ts-mode)
        (js2-mode . javascript-ts-mode)
        (nix-mode . nix-ts-mode)))

(require 'eglot)

(add-hook 'python-ts-mode-hook 'eglot-ensure)
(add-hook 'java-ts-mode-hook 'eglot-ensure)
(add-hook 'c-ts-mode-hook 'eglot-ensure)
(add-hook 'c++-ts-mode-hook 'eglot-ensure)
(add-hook 'yaml-ts-mode-hook 'eglot-ensure)
(add-hook 'go-ts-mode-hook 'eglot-ensure)
(add-hook 'lua-ts-mode-hook 'eglot-ensure)
(add-hook 'javascript-ts-mode-hook 'eglot-ensure)
(add-hook 'typescript-ts-mode-hook 'eglot-ensure)
(add-hook 'rust-ts-mode-hook 'eglot-ensure)
(add-hook 'ruby-ts-mode-hook 'eglot-ensure)
(add-hook 'r-ts-mode-hook 'eglot-ensure)
(add-hook 'julia-ts-mode-hook 'eglot-ensure)

(define-key eglot-mode-map (kbd "C-c C-a") 'eglot-code-actions)

(use-package cmake-mode :ensure t)

(use-package nix-mode :ensure t :mode "\\.nix\\'")

;; Github Copilot
(use-package s :ensure t)
(use-package dash :ensure t)
(use-package editorconfig :ensure t)
(add-hook 'prog-mode-hook #'(lambda ()
                              (require 'copilot)))
(with-eval-after-load 'copilot
  (defun zelcon/copilot-tab ()
    (interactive)
    (if (and (bound-and-true-p copilot-mode)
             (functionp 'copilot-completion-active-p)
             (copilot-completion-active-p))
        (or (copilot-accept-completion)
            (indent-for-tab-command))
      (indent-for-tab-command)))
  (setq warning-minimum-level :error)
  (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "C-TAB") 'copilot-accept-completion-by-word)
  (define-key copilot-completion-map (kbd "C-<tab>") 'copilot-accept-completion-by-word)
  (evil-define-key 'insert 'global (kbd "<tab>") 'zelcon/copilot-tab))


(use-package spinner :ensure t)

(use-package ellama :ensure t
  :init
  (require 'llm-ollama)
  (setopt ellama-provider
          (make-llm-ollama
           :chat-model "deepseek-coder:6.7b-instruct"
           :embedding-model "deepseek-coder:6.7b-instruct"))
  (setopt ellama-providers
          '(("codellama" . (make-llm-ollama
                            :chat-model "codellama:70b"
                            :embedding-model "codellama:70b"))
            ("deepseek" . (make-llm-ollama
                           :chat-model "deepseek-coder:6.7b-instruct"
                           :embedding-model "deepseek-coder:6.7b-instruct"))
            ("mixtral" . (make-llm-ollama
                          :chat-model "mixtral:8x7b-instruct-v0.1-q3_K_M-4k"
                          :embedding-model "mixtral:8x7b-instruct-v0.1-q3_K_M-4k")))))

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

(setq completion-styles '(flex partial-completion substring initials basic))

  

;;
;; Appearance
;; ----------
;;

;; theme
(use-package solarized-theme
  :ensure t
  :config
  (load-theme 'solarized-dark t))

(use-package challenger-deep-theme
  :disabled
  :ensure t
  :config
  (load-theme 'challenger-deep t))

;; frame size
(defun zelcon/set-frame-size-according-to-resolution ()
  (interactive)
  (when window-system
    (if (> (x-display-pixel-width) 1500)
        (progn (set-frame-size (selected-frame) 120 65)
               (add-to-list 'default-frame-alist '(height . 65))
               (add-to-list 'default-frame-alist '(width . 120)))
      (progn (set-frame-size (selected-frame) 85 48)
                (add-to-list 'default-frame-alist '(height . 48))
                (add-to-list 'default-frame-alist '(width . 85))))))
(add-hook 'after-init-hook 'zelcon/set-frame-size-according-to-resolution)


;; line numbers
(global-display-line-numbers-mode)
(setq display-line-numbers-type 'relative)

;; indent guides
(require 'indent-bars)
(when (eq system-type 'darwin)
    (setq indent-bars-prefer-character t))
(setq indent-bars-treesit-support t)
(setq indent-bars-no-descend-string t)
(setq indent-bars-treesit-ignore-blank-lines-types '("module"))
(setq indent-bars-treesit-wrap '((python argument_list parameters ; for python, as an example
                                            list list_comprehension
                                            dictionary dictionary_comprehension
                                            parenthesized_expression subscript)))
(add-hook 'python-ts-mode-hook 'indent-bars-mode)
(add-hook 'yaml-ts-mode-hook 'indent-bars-mode)

;; truncate lines
(setq-default truncate-lines t)

;; scroll horizontally one column at a time
(setq hscroll-step 1)

;; save place
(save-place-mode 1)
(setq save-place-file (concat user-emacs-directory "places")
      save-place-forget-unreadable-files nil
      save-place-limit 10000
      save-place-version-control t
      save-place-save-skipped nil)

;; hide ugly buttons on the toolbar
(tool-bar-mode -1)

;; treemacs
(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))

  :config
  (progn
    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    )
  :bind
  (:map global-map
        ("M-0" . treemacs-select-window)
        ("C-x t t" . treemacs)
        ("C-x t B" . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag))
  )
(use-package treemacs-evil
  :ensure t
  :after (treemacs evil))


(setq package-install-upgrade-built-in t)

;; load custom file
(load custom-file t)
