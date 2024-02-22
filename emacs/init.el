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

;; utils

(defun zelcon/alist-find (alist elt)
  (let ((found nil))
    (dolist (pair eglot-server-programs)
      (when (or (and (listp (car pair)) (member elt (car pair)))
                (eq (car pair) elt))
        (setq found pair)))
    found))

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

(setq evil-want-C-i-jump t)

;; set some modes to use emacs mode by default
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
(add-hook 'special-mode-hook 'evil-emacs-state)
(evil-set-initial-state 'messages-buffer-mode 'emacs)
(with-current-buffer (get-buffer "*Messages*")
  (evil-emacs-state))

(evil-define-key 'normal 'global (kbd "SPC i") 'imenu)
(evil-define-key 'normal 'global (kbd "SPC o") 'other-window)

;
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

;; completion
(use-package company
  :ensure t
  :config
  (global-company-mode)
  (setq company-idle-delay 0.1))
(use-package company-box
  :ensure t
  :hook (company-mode . company-box-mode))

;;
;; Fixing Annoying Defaults
;; ------------------------
;;

;;; tramp
;;; http://stackoverflow.com/questions/13794433/how-to-disable-autosave-for-tramp-buffers-in-emacs
(setq tramp-auto-save-directory "/tmp")

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

;; refresh open files to latest version on disk automtically
;; useful when an external program modified a file; e.g., `clang-format`
(global-auto-revert-mode)

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
        (commonlisp "https://github.com/tree-sitter-grammars/tree-sitter-commonlisp")
        (cuda "https://github.com/theHamsta/tree-sitter-cuda")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")
        (starlark "https://github.com/amaanq/tree-sitter-starlark")
        (lua "https://github.com/tree-sitter-grammars/tree-sitter-lua")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (cmake "https://github.com/uyha/tree-sitter-cmake")
        (make "https://github.com/alemuller/tree-sitter-make")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (swift "https://github.com/alex-pinkus/tree-sitter-swift")
        (toml "https://github.com/ikatyang/tree-sitter-toml")
        (latex "https://github.com/latex-lsp/tree-sitter-latex")
        (rust "https://github.com/tree-sitter/tree-sitter-rust")
        (ruby "https://github.com/tree-sitter/tree-sitter-ruby")
        (r "https://github.com/r-lib/tree-sitter-r")
        (make "https://github.com/alemuller/tree-sitter-make")
        (julia "https://github.com/tree-sitter/tree-sitter-julia")
        (rst "https://github.com/stsewd/tree-sitter-rst")
        (markdown "https://github.com/ikatyang/tree-sitter-markdown")
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

(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))
(add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-ts-mode))
(add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-ts-mode))
(add-to-list 'auto-mode-alist '("\\.jl\\'" . julia-ts-mode))
(add-to-list 'auto-mode-alist '("\\.r\\'" . r-ts-mode))
(add-to-list 'auto-mode-alist '("\\.rb\\'" . ruby-ts-mode))
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-ts-mode))
(add-to-list 'auto-mode-alist '("\\.java\\'" . java-ts-mode))

(use-package paredit
  :ensure t
  :hook '((emacs-lisp-mode . paredit-mode)
          (common-lisp-mode . paredit-mode)
          (clojure-mode . paredit-mode)
          (scheme-mode . paredit-mode)
          (racket-mode . paredit-mode)
          (lisp-mode . paredit-mode))
  )

;; Language Server Protocol (LSP)

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

(defun zelcon/clear-eglot-server-program (mode-name)
  (setq eglot-server-programs
        (assoc-delete-all mode-name
                          eglot-server-programs
                          (lambda (alist-key our-key)
                            (if (listp alist-key)
                                (member our-key alist-key)
                              (equal our-key alist-key))))))

(zelcon/clear-eglot-server-program 'rust-ts-mode)
(add-to-list 'eglot-server-programs
             '((rust-ts-mode rust-mode) .
               ("rust-analyzer" :initializationOptions (:check (:command "clippy")))))



(use-package cmake-mode :ensure t)

(use-package nix-mode :ensure t :mode "\\.nix\\'")

(use-package markdown-mode :ensure t)

;; Github Copilot
(use-package s :ensure t)
(use-package dash :ensure t)
(use-package editorconfig :ensure t)
(use-package copilot
  :ensure nil
  :requires (s dash editorconfig)
  :defer t
  :after (evil jsonrpc)
  :init
  (unless (package-installed-p 'copilot)
    (package-vc-install "https://github.com/copilot-emacs/copilot.el.git"))
  :hook ((python-ts . copilot-mode)
         (rust-ts . copilot-mode)
         (java-ts . copilot-mode)
         (c++-ts . copilot-mode)
         (c-ts . copilot-mode)
         (go-ts . copilot-mode)
         (javascript-ts . copilot-mode)
         (typescript-ts . copilot-mode)
         (ruby-ts . copilot-mode)
         (swift-ts . copilot-mode)
         (julia-ts . copilot-mode)
         (bash-ts . copilot-mode)
         (shell-script . copilot-mode))
  :config
  (add-to-list 'warning-suppress-types '(copilot copilot-no-mode-indent))
  (defun zelcon/copilot-tab ()
    "Accept copilot or company completion, or indent if no completion is available."
    (interactive)
    (or (copilot-accept-completion)
        (company-indent-or-complete-common nil)))
  (evil-define-key 'insert 'copilot-mode-map
    (kbd "<tab>") 'zelcon/copilot-tab
    (kbd "C-j") 'copilot-next-completion
    (kbd "C-k") 'copilot-previous-completion
    (kbd "C-<tab>") 'copilot-accept-completion-by-word
    (kbd "ESC") 'copilot-clear-overlay)
  ;; https://code.visualstudio.com/docs/languages/identifiers#_known-language-identifiers
  (nconc copilot-major-mode-alist '(("python-ts" . "python")
                                    ("rust-ts" . "rust")
                                    ("java-ts" . "java")
                                    ("c++-ts" . "cpp")
                                    ("c-ts" . "c")
                                    ("go-ts" . "go")
                                    ("javascript-ts" . "javascript")
                                    ("typescript-ts" . "typescript")
                                    ("ruby-ts" . "ruby")
                                    ("swift-ts" . "swift")
                                    ("yaml-ts" . "yaml")
                                    ("julia-ts" . "julia"))))


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
                          :embedding-model "mixtral:8x7b-instruct-v0.1-q3_K_M-4k"))))
  :config
  (setq ellama-keymap-prefix "C-c e"))

;; completions
(setq completions-format 'one-column)
(setq completions-header-format nil)
(setq completions-max-height 10)
(setq completion-auto-select nil)
(define-key minibuffer-local-completion-map (kbd "C-n") 'minibuffer-next-line-completion)
(define-key minibuffer-local-completion-map (kbd "C-p") 'minibuffer-previous-line-completion)
(define-key global-map (kbd "C-c b") 'switch-to-minibuffer)

;; Interactively Do Things -- fast buffer switch
(require 'ido)
(ido-mode 1)
(setq ido-ignore-buffers '("^ " "*Completions*" "*Shell Command Output*"
               "*Messages*" "Async Shell Command"))
(ido-everywhere 1)
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
        (progn (set-frame-size (selected-frame) 140 180)
               (add-to-list 'default-frame-alist '(height . 180))
               (add-to-list 'default-frame-alist '(width . 140)))
      (progn (set-frame-size (selected-frame) 80 65)
                (add-to-list 'default-frame-alist '(height . 80))
                (add-to-list 'default-frame-alist '(width . 65))))))
(add-hook 'after-init-hook 'zelcon/set-frame-size-according-to-resolution)


;; line numbers
(global-display-line-numbers-mode)
(setq display-line-numbers-type 'relative)

;; indent guides
(use-package indent-bars
  :ensure nil
  :init
  (unless (package-installed-p 'indent-bars)
    (package-vc-install "https://github.com/jdtsmith/indent-bars.git"))
  :config
  (when (eq system-type 'darwin)
    (setq indent-bars-prefer-character t))
  :custom
  (indent-bars-treesit-support t)
  (indent-bars-treesit-ignore-blank-lines-types '("module"))
  (indent-bars-treesit-wrap '((python argument_list parameters ; for python, as an example
                                      list list_comprehension
                                      dictionary dictionary_comprehension
                                      parenthesized_expression subscript)
                              (c argument_list parameter_list init_declarator))))

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

;; Yasnippet
(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

(setq package-install-upgrade-built-in t)

(setq load-prefer-newer t)

;; load custom file
(load custom-file t)
