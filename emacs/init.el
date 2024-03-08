;;
;; Initial Setup
;; -------------
;;

;;; MELPA

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
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
     (cond
      ;; Wayland
      ((and (string-equal (getenv "WAYLAND_DISPLAY") "wayland")
            (executable-find "wl-copy")
            (executable-find "wl-paste"))
       (progn
         (setq interprogram-cut-function
               (lambda (text &optional push)
                 (let* ((process-connection-type nil)
                        (proc (start-process "wl-copy" "*Messages*" "wl-copy")))
                   (process-send-string proc text)
                   (process-send-eof proc))))
         (setq interprogram-paste-function
               (lambda (text &optional push)
                 (shell-command-to-string "wl-paste -n")))))
      ;; X11
       ((and (not (null window-system)) (executable-find "xsel"))
         (progn
           (setq interprogram-cut-function
                 (lambda (text &optional push)
                   (let* ((process-connection-type nil)
                          (proc (start-process "xsel" "*Messages*" "xsel" "-i" "-b")))
                     (process-send-string proc text)
                     (process-send-eof proc))))
           (setq interprogram-paste-function
                 (lambda ()
                   (shell-command-to-string "xsel -o -b")))))
       ;; tmux
        ((and (getenv "TMUX") (executable-find "tmux"))
          (setq interprogram-cut-function
                (lambda (text &optional push)
                  (let* ((process-connection-type nil)
                         (proc (start-process "tmux" "*Messages*" "tmux" "load-buffer" "-")))
                    (process-send-string proc text)
                    (process-send-eof proc))))
          (setq interprogram-paste-function
                (lambda ()
                  (shell-command-to-string "tmux paste-buffer")))))))

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
(evil-set-initial-state 'org-mode 'emacs)
(add-hook 'special-mode-hook 'evil-emacs-state)
(evil-set-initial-state 'messages-buffer-mode 'emacs)
(with-current-buffer (get-buffer "*Messages*")
  (evil-emacs-state))

(evil-define-key 'normal 'global (kbd "SPC i") 'imenu)
(evil-define-key 'normal 'global (kbd "SPC o") 'other-window)

;; Allows you to click buttons without initiating a selection
(define-key evil-motion-state-map [down-mouse-1] nil)


;;
;; Git
;; ---
;;

(use-package magit :ensure t)

;
;; Navigation
;; ----------
;;

;; 4 width spaces as indentation
(setopt standard-indent 4
        tab-width 4
        indent-tabs-mode nil
        c-basic-indent 4)

;; Customize word boundaries to treat '_' as part of words
(modify-syntax-entry ?_ "w")

;; Customize word boundaries to treat '-' as part of words
(modify-syntax-entry ?- "w")

;; Customize word boundaries to treat '/' as part of words
(modify-syntax-entry ?/ "w")
(put 'upcase-region 'disabled nil)

;; move region
(require 'move-region)
(evil-define-key 'visual 'global (kbd "M-S-<down>") 'zelcon/move-region-down)
(evil-define-key 'visual 'global (kbd "M-S-<up>") 'zelcon/move-region-up)

;; move lines
(require 'move-lines)
(evil-define-key 'normal 'global (kbd "M-S-<up>") 'zelcon/move-line-up)
(evil-define-key 'normal 'global (kbd "M-S-<down>") 'zelcon/move-line-down)

;; which-key
(use-package which-key :ensure t
  :config
  (add-hook 'after-init-hook 'which-key-mode))

;; ace-jump-mode
(use-package ace-jump-mode
  :ensure t
  :after evil
  :config
  (define-key evil-motion-state-map "g/" 'evil-ace-jump-word-mode)
  (define-key evil-motion-state-map "g." 'evil-ace-jump-char-mode))

;; expand-region which uses tree-sitter
(require 'expreg)
(define-key global-map (kbd "C-;") 'expreg-expand)
(define-key global-map (kbd "C-:") 'expreg-contract)

;; multiple-cursors
(use-package evil-mc
  :ensure t
  :config
  (global-evil-mc-mode)
  (global-set-key (kbd "s-<down-mouse-1>") 'evil-mc-toggle-cursor-on-click))

;; emulates the legendary surround.vim
;; You can surround in visual-state with `S<textobject>` or `gS<textobject>`. Or in normal-state with `ys<textobject>` or `yS<textobject>`.
;; You can change a surrounding with `cs<old-textobject><new-textobject>`.
;; You can delete a surrounding with `ds<textobject>`.
(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

;;
;; Fixing Annoying Defaults
;; ------------------------
;;

(setopt

 tramp-auto-save-directory "/tmp"

 ;; "yes or no" ➙ "y or n"
 use-short-answers t

 ring-bell-function 'ignore

 inhibit-start-screen t
 inhibit-startup-screen t

 apropos-do-all t

 ;; save whatever's in the clipboard before replacing it
 save-interprogram-paste-before-kill t

 ;; Do not show the prompt: "Symbolic link to Git-controlled source file; follow link (y or n)"
 vc-follow-symlinks t

 ;; recent files
 recentf-max-saved-items 200
 recentf-max-menu-items 200

 sentence-end-double-space nil

 ;; prevent creation of junk tilde files
 ;; http://stackoverflow.com/questions/13794433/how-to-disable-autosave-for-tramp-buffers-in-emacs
 backup-directory-alist
 `(("." . ,(concat user-emacs-directory "backups")))
 ;; sane backup settings
 backup-by-copying t
 version-control t
 delete-old-versions t
 kept-old-versions 100
 create-lockfiles nil

 ;; handle C-h on terminals; maps help-command to <f1>; keeeps
 ;; backspace behavior of C-h
 normal-erase-is-backspace t
 )

;; ensure that auto-save files end up in the right place
(append auto-save-file-name-transforms
        `((".*" ,(concat user-emacs-directory "backups") t)))

;; refresh open files to latest version on disk automtically
;; useful when an external program modified a file; e.g., `clang-format`
(global-auto-revert-mode)
;; Revert Dired buffers too
(setopt global-auto-revert-non-file-buffers t)

;; In the minibuffer, make C-h do backspace, not help
(define-key minibuffer-local-map (kbd "C-h") 'delete-backward-char)

;; hide ugly buttons on the toolbar
(tool-bar-mode -1)


;;
;; Org Mode
;; --------
;;

(require 'org)

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
        (cuda "https://github.com/theHamsta/tree-sitter-cuda")
        (commonlisp "https://github.com/tree-sitter-grammars/tree-sitter-commonlisp")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (go-mod "https://github.com/camdencheek/tree-sitter-go-mod")
        (go-work "https://github.com/omertuc/tree-sitter-go-work")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")
        (starlark "https://github.com/amaanq/tree-sitter-starlark")
        (lua "https://github.com/tree-sitter-grammars/tree-sitter-lua")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (cmake "https://github.com/uyha/tree-sitter-cmake")
        (make "https://github.com/alemuller/tree-sitter-make")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (swift "https://github.com/alex-pinkus/tree-sitter-swift")
        (toml "https://github.com/ikatyang/tree-sitter-toml")
        (latex "https://github.com/latex-lsp/tree-sitter-latex")
        (rust "https://github.com/tree-sitter/tree-sitter-rust")
        (ruby "https://github.com/tree-sitter/tree-sitter-ruby")
        (make "https://github.com/alemuller/tree-sitter-make")
        (jq "https://github.com/flurie/tree-sitter-jq")
        (julia "https://github.com/tree-sitter/tree-sitter-julia")
        (rst "https://github.com/stsewd/tree-sitter-rst")
        (markdown "https://github.com/ikatyang/tree-sitter-markdown")
        (nix "https://github.com/nix-community/tree-sitter-nix")
        (clojure "https://github.com/sogaiu/tree-sitter-clojure")
        (proto "https://github.com/mitchellh/tree-sitter-proto")
        (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
        (verilog "https://github.com/tree-sitter/tree-sitter-verilog")))


;; Remap common major modes to tree-sitter versions
(add-to-list 'major-mode-remap-alist '(js-json-mode . json-ts-mode))
(add-to-list 'major-mode-remap-alist '(yaml-mode . yaml-ts-mode))
(add-to-list 'major-mode-remap-alist '(java-mode . java-ts-mode))
(add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
(add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))


;; Associate file extensions
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))
(add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-ts-mode))
(add-to-list 'auto-mode-alist '("\\.jl\\'" . julia-ts-mode))
(add-to-list 'auto-mode-alist '("\\.rb\\'" . ruby-ts-mode))
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))
(add-to-list 'auto-mode-alist '("\\.go.mod\\'" . go-mod-ts-mode))
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-ts-mode))
(add-to-list 'auto-mode-alist '("\\.java\\'" . java-ts-mode))
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-ts-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))


(defun zelcon/install-tree-sitter-langs ()
  "Install all tree-sitter languages. Typically you only need to run
this once."
  (interactive)
  (mapc #'treesit-install-language-grammar
        (mapcar #'car treesit-language-source-alist)))

;; Lisp
(use-package slime :ensure t)
(setq inferior-lisp-program "sbcl")
;; structural editing for S-expressions
(use-package paredit
  :ensure t
  :hook '((emacs-lisp-mode . paredit-mode)
          (common-lisp-mode . paredit-mode)
          (clojure-mode . paredit-mode)
          (scheme-mode . paredit-mode)
          (racket-mode . paredit-mode)
          (lisp-mode . paredit-mode)))

;; Language Server Protocol (LSP)

(require 'eglot)

(add-hook 'python-ts-mode-hook 'eglot-ensure)
(add-hook 'java-ts-mode-hook 'eglot-ensure)
(add-hook 'c-ts-mode-hook 'eglot-ensure)
(add-hook 'c++-ts-mode-hook 'eglot-ensure)
(add-hook 'yaml-ts-mode-hook 'eglot-ensure)
(add-hook 'yaml-mode-hook 'eglot-ensure)
(add-hook 'go-ts-mode-hook 'eglot-ensure)
(add-hook 'lua-ts-mode-hook 'eglot-ensure)
(add-hook 'javascript-ts-mode-hook 'eglot-ensure)
(add-hook 'typescript-ts-mode-hook 'eglot-ensure)
(add-hook 'rust-ts-mode-hook 'eglot-ensure)
(add-hook 'ruby-ts-mode-hook 'eglot-ensure)
(add-hook 'r-ts-mode-hook 'eglot-ensure)
(add-hook 'julia-ts-mode-hook 'eglot-ensure)

(define-key eglot-mode-map (kbd "C-c C-a") 'eglot-code-actions)

(evil-define-key 'normal eglot-mode-map (kbd "SPC r") 'eglot-rename)
(evil-define-key 'normal eglot-mode-map (kbd "SPC f") 'eglot-format-buffer)
(evil-define-key 'normal eglot-mode-map (kbd "SPC a") 'eglot-code-actions)

(evil-define-key 'normal eglot-mode-map (kbd "SPC <down>") 'flymake-goto-next-error)
(evil-define-key 'normal eglot-mode-map (kbd "SPC <up>") 'flymake-goto-prev-error)

(defun zelcon/clear-eglot-server-program (mode-name)
  (setq eglot-server-programs
        (assoc-delete-all mode-name
                          eglot-server-programs
                          (lambda (alist-key our-key)
                            (if (listp alist-key)
                                (member our-key alist-key)
                              (equal our-key alist-key))))))

;; Rust
(zelcon/clear-eglot-server-program 'rust-ts-mode)
(add-to-list 'eglot-server-programs
             '((rust-ts-mode rust-mode) .
               ("rust-analyzer" :initializationOptions (:check (:command "clippy")))))

;; CMake
(use-package cmake-mode :ensure t)

;; Nix
(use-package nix-ts-mode :ensure t :mode "\\.nix\\'"
  :config
  (add-to-list 'eglot-server-programs
               '(nix-ts-mode (eglot-alternatives '("nixd" "rnix-lsp")))))

;; Clojure
(use-package clojure-ts-mode :ensure t)

;; Verilog
(use-package verilog-ts-mode :ensure t)

;; Julia
(use-package julia-ts-mode :ensure t)

;; Markdown
(use-package markdown-mode :ensure t)

;; Protocol Buffers
(use-package protobuf-mode
  :ensure t
  :defer t
  )

;; Gherkin
(use-package feature-mode
  :ensure t
  :defer t
  :config
  (require 'org)
  (when (boundp evil-mode)
    (evil-set-initial-state 'feature-mode 'emacs)))
 
;;;
;;; Completion
;;; ----------
;;;

(use-package company
  :ensure t
  :config
  (define-key company-active-map (kbd "<tab>") nil)
  ;; disable inline previews
  (delq 'company-preview-if-just-one-frontend company-frontends)
  ;; escape from company completions
  (evil-define-key 'insert company-active-map (kbd "ESC") 'company-abort)
  :hook (prog-mode . company-mode)
  :custom
  (company-idle-delay 0.3))
(use-package company-box
    :ensure t
    :when (not (null window-system))
    :hook (company-mode . company-box-mode))

;; Github Copilot
(use-package s :ensure t)
(use-package dash :ensure t)
(use-package editorconfig :ensure t)
(use-package copilot
  :ensure nil
  :requires (s dash editorconfig)
  :after (evil company)
  :init
  (unless (package-installed-p 'copilot.el)
    (package-vc-install "https://github.com/copilot-emacs/copilot.el.git"))
  :hook
  ((python-ts-mode . copilot-mode)
   (rust-ts-mode . copilot-mode)
   (java-ts-mode . copilot-mode)
   (c++-ts-mode . copilot-mode)
   (c-ts-mode . copilot-mode)
   (go-ts-mode . copilot-mode)
   (javascript-ts-mode . copilot-mode)
   (tsx-ts-mode . copilot-mode)
   (typescript-ts-mode . copilot-mode)
   (ruby-ts-mode . copilot-mode)
   (swift-ts-mode . copilot-mode)
   (julia-ts-mode . copilot-mode)
   (bash-ts-mode . copilot-mode)
   (shell-script-mode . copilot-mode))
  :config
  (add-to-list 'warning-suppress-types '(copilot copilot-no-mode-indent copilot-exceeds-max-char))
  (defun zelcon/copilot-tab ()
    "Accept copilot or company completion, or indent if no completion is available."
    (interactive)
    (or (copilot-accept-completion)
        (company-indent-or-complete-common nil)
        (evil-insert 1)
        (indent-for-tab-command)))
  (defun zelcon/copilot-c-tab ()
    (interactive)
    (or (copilot-accept-completion-by-line)
        (evil-insert 1)
        (indent-for-tab-command)))
  (define-key company-active-map (kbd "TAB") 'zelcon/copilot-tab)
  (define-key company-active-map (kbd "<tab>") 'zelcon/copilot-tab)
  (define-key copilot-completion-map (kbd "TAB") 'zelcon/copilot-tab)
  (define-key copilot-completion-map (kbd "<tab>") 'zelcon/copilot-tab)
  (define-key copilot-completion-map (kbd "C-M-<tab>") 'zelcon/copilot-c-tab)
  (define-key copilot-completion-map (kbd "C-M-TAB") 'zelcon/copilot-c-tab)
  (define-key copilot-mode-map (kbd "C-'") 'copilot-accept-completion-by-word)
  (define-key copilot-completion-map (kbd "<backtab>") 'copilot-previous-completion)
  (define-key copilot-completion-map (kbd "<f7>") 'copilot-accept-completion-by-word)
  (define-key copilot-completion-map (kbd "<f8>") 'copilot-accept-completion-by-line)
  ;; https://code.visualstudio.com/docs/languages/identifiers#_known-language-identifiers
  (dolist (item '(("python-ts" . "python")
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
                  ("julia-ts" . "julia")))
    (add-to-list 'copilot-major-mode-alist item))
  )

(use-package spinner :ensure t)

(use-package ellama :ensure t
  :requires spinner
  :init
  (require 'llm-ollama)
  (setopt ellama-provider
          (make-llm-ollama
           :chat-model "deepseek-coder:33b-instruct"
           :embedding-model "deepseek-coder:33b-instruct"))
  (setopt ellama-providers
          '(("wizardcoder" . (make-llm-ollama
                             :chat-model "wizardcoder:33b-v1.1"
                             :embedding-model "wizardcoder:33b-v1.1"))
            ("codellama" . (make-llm-ollama
                            :chat-model "codellama:70b"
                            :embedding-model "codellama:70b"))
            ("deepseek-big" . (make-llm-ollama
                               :chat-model "deepseek-coder:33b-instruct"
                               :embedding-model "deepseek-coder:33b-instruct"))
            ("deepseek-small" . (make-llm-ollama
                                 :chat-model "deepseek-coder:6.7b-instruct"
                                 :embedding-model "deepseek-coder:6.7b-instruct"))))
  :config
  (setq ellama-keymap-prefix "C-c e"))

;; minibuffer completions
(fido-mode 1)
(fido-vertical-mode 1)
(setq completion-styles '(flex partial-completion substring initials basic))
(setopt completions-format 'one-column
        completions-header-format nil
        completions-max-height 10
        completion-auto-select nil
        fido-vertical-mode-show-count t)
;; Interactively Do Things -- fast buffer switch
(setq ido-ignore-buffers '("^ " "*Completions*" "*Shell Command Output*"
               "*Messages*" "Async Shell Command"))

 

;; searching
(setopt
 case-fold-search t ;; case-insensitive search
 )
(defun zelcon/isearch-region-or-thing-at-point ()
  "If a region is active, search for the contents of the region. Otherwise, search for the symbol at point."
  (interactive)
  (if (use-region-p)
      (let ((search-term (buffer-substring-no-properties (region-beginning) (region-end))))
        (deactivate-mark)
        (isearch-forward search-term))
    (isearch-forward-thing-at-point)))
(evil-define-key '(normal) 'global (kbd "SPC s") 'zelcon/isearch-region-or-thing-at-point)


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

;; always highlight current line
(global-hl-line-mode)

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

;; column numbers
(add-hook 'after-init-hook 'column-number-mode)

;; indent guides
(use-package indent-bars
  :ensure nil
  :init
  (unless (package-installed-p 'indent-bars)
    (package-vc-install "https://github.com/jdtsmith/indent-bars.git"))
  :config
  (when (eq system-type 'darwin)
    (setq indent-bars-prefer-character t))
  :hook ((python-ts-mode . indent-bars-mode)
         (yaml-ts-mode . indent-bars-mode)
         (js-json-mode . indent-bars-mode)
         (tsx-ts-mode . indent-bars-mode)
         (json-ts-mode . indent-bars-mode)
         (nxml-mode . indent-bars-mode))
  :custom
  (indent-bars-treesit-support (treesit-available-p))
  (indent-bars-treesit-ignore-blank-lines-types '("module"))
  (indent-bars-treesit-wrap '((python argument_list parameters ; for python, as an example
                                      list list_comprehension
                                      dictionary dictionary_comprehension
                                      parenthesized_expression subscript)
                              (c argument_list parameter_list init_declarator))))

;; truncate lines
(setq-default truncate-lines t)

;; scroll horizontally one column at a time
(setopt hscroll-step 1
        scroll-step 1
        scroll-conservatively 10000
        scroll-margin 1
        scroll-preserve-screen-position 1)

;; display time and date
(add-hook 'after-init-hook 'display-time-mode)
(setopt display-time-day-and-date t)

;; save place
(save-place-mode 1)
(setq save-place-file (concat user-emacs-directory "places")
      save-place-forget-unreadable-files t
      save-place-limit 10000
      save-place-version-control t
      save-place-save-skipped nil)

;; Tab Bars
(tab-bar-mode)
(defun zelcon/disable-tab-bar-for-company-box ()
  "Disables tab bar-mode if the current frame is the company-box frame."
  (when (and (bound-and-true-p tab-bar-mode)
             (bound-and-true-p company-box-mode)
             (equal (selected-frame)
                    (company-box--get-frame)))
    (tab-bar-mode -1)))
(add-hook 'after-make-frame-functions 'zelcon/disable-tab-bar-for-company-box)
(global-unset-key (kbd "s-t"))
(global-set-key (kbd "s-t") 'tab-bar-new-tab)
(global-unset-key (kbd "s-w"))
(global-set-key (kbd "s-w") 'tab-bar-close-tab)

;; highlight symbol
(use-package highlight-symbol
  :ensure t
  :custom
  (highlight-symbol-idle-delay 1.5))

;; Yasnippet
(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

(setq package-install-upgrade-built-in t)

(setq load-prefer-newer t)

(load "format-sql.el")

;; load custom file
(load custom-file t)
