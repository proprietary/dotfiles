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
(let ((default-directory (expand-file-name
            (concat user-emacs-directory
              "per-machine-lisp"))))
  (when (file-exists-p default-directory)
    (normal-top-level-add-to-load-path '("."))
    (normal-top-level-add-subdirs-to-load-path)
    (dolist (file (directory-files default-directory t "\\.el$"))
      (load file))))

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
      (progn
        (setq interprogram-cut-function
              (lambda (text &optional push)
                (let* ((process-connection-type nil)
                       (proc (start-process "tmux" "*Messages*" "tmux" "load-buffer" "-b" "emacs" "-")))
                  (process-send-string proc text)
                  (process-send-eof proc))))
        (setq interprogram-paste-function
              (lambda ()
                (shell-command-to-string "tmux save-buffer -b emacs -")))))))
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
;; (when (memq window-system '(mac ns x))
;;   (require 'exec-path-from-shell)
;;   (exec-path-from-shell-initialize))

(when (eq system-type 'darwin)
  (if-let ((gnu-ls (executable-find "gls")))
      (setopt dired-use-ls t
        insert-directory-program gnu-ls
        dired-listing-switches "-aBhl --group-directories-first")
    (setopt dired-use-ls-dired nil)))

;;
;; Direnv
;; ------
(use-package direnv :ensure t)

;; Projectile
;; ----------
;;
(use-package projectile :ensure t
  :config
  (global-unset-key (kbd "C-x p"))
  (define-key projectile-mode-map (kbd "C-x p") 'projectile-command-map)
  (projectile-mode +1))


;; hitting backspace at an indentation column with whitespace
;; preceding the cursor also deletes backward up to the last
;; indentation column
(use-package hungry-delete :ensure t
  :config
  (global-hungry-delete-mode)
  (setopt hungry-delete-join-reluctantly t))

(use-package browse-kill-ring
  :ensure t
  :bind (("C-c y" . browse-kill-ring)))
;; Prevent whitespace from showing up in the kill ring
(setq kill-transform-function (lambda (s) (unless (string-blank-p s) s)))

;;
;; Git
;; ---
;;

(use-package magit :ensure t :defer t)

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
(global-set-key (kbd "M-S-<down>") 'zelcon/move-region-down)
(global-set-key (kbd "M-S-<up>") 'zelcon/move-region-up)

;; move lines
(require 'move-lines)
(global-set-key (kbd "M-S-<up>") 'zelcon/move-line-up)
(global-set-key (kbd "M-S-<down>") 'zelcon/move-line-down)

;; which-key
(use-package which-key :ensure t
  :config
  (add-hook 'after-init-hook 'which-key-mode))

;; ;; ace-jump-mode
;; (use-package ace-jump-mode
;;   :ensure t
;;   :after evil
;;   :config
;;   (define-key evil-motion-state-map "g/" 'evil-ace-jump-word-mode)
;;   (define-key evil-motion-state-map "g." 'evil-ace-jump-char-mode))

;; expand-region which uses tree-sitter
(require 'expreg)
(define-key global-map (kbd "C-;") 'expreg-expand)
(define-key global-map (kbd "C-:") 'expreg-contract)

;; multiple-cursors
;; (use-package evil-mc
;;   :ensure t
;;   :bind (:map evil-mc-key-map
;;               ("M-n" . nil)
;;               ("M-p" . nil)
;;               ("C-n" . nil)
;;               ("C-t" . nil)
;;               ("C-p" . nil))
;;   :config
;;   (global-evil-mc-mode)
;;   (global-set-key (kbd "s-<down-mouse-1>") 'evil-mc-toggle-cursor-on-click)
;;   (evil-define-key 'normal evil-mc-key-map (kbd "g r ]") 'evil-mc-make-and-goto-next-match)
;;   (evil-define-key 'normal evil-mc-key-map (kbd "g r [") 'evil-mc-make-and-goto-prev-match)
;;   (evil-define-key 'normal evil-mc-key-map (kbd "g r \\") 'evil-mc-skip-and-goto-next-match))
;;
;; Fixing Annoying Defaults
;; ------------------------
;;

(if (and (null window-system)
     (eq system-type 'darwin))
  (normal-erase-is-backspace-mode 0)
  (normal-erase-is-backspace-mode 1))

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

 ;; Don't create those '#filename.c#' lock files (🤮🗑️)
 create-lockfiles nil
 remote-file-name-inhibit-locks t

 ;; send deleted files to system Trash instead of physically deleting them
 delete-by-moving-to-trash t

 ;; yes, emacs, I'm not running a Pentium IV
 large-file-warning-threshold (ash 1 30)

 ;; prevent dired buffer clutter
 ;dired-kill-when-opening-new-dired-buffer t
 )

;; allow use of dired-find-alternate-file
(put 'dired-find-alternate-file 'disabled nil)

;; ensure that auto-save files end up in the right place
(append auto-save-file-name-transforms
  `((".*" ,(concat user-emacs-directory "backups") t)))

;; refresh open files to latest version on disk automtically
;; useful when an external program modified a file; e.g., `clang-format`
(global-auto-revert-mode)
;; Revert Dired buffers too
(setopt global-auto-revert-non-file-buffers t)

;; hide ugly buttons on the toolbar
(tool-bar-mode -1)

;; clean up whitespace
(add-hook 'before-save-hook
    (lambda ()
      (unless (member major-mode
          '(org-mode
            markdown-mode
            gfm-mode
            makefile-mode
            makefile-ts-mode
            makefile-gmake-mode
            makefile-bsdmake-mode
            makefile-automake-mode
            makefile-imake-mode
            makefile-makepp-mode
            makefile-cmake-mode
            makefile-bsdmake-mode
            cmake-mode
            cmake-ts-mode
            feature-mode))
        (whitespace-cleanup))))

(normal-erase-is-backspace-mode -1)

;; Auto-approve .dir-locals.el
;; Warning: This may be inscecure
(setopt enable-local-variables :all)

;;;;;;;;;
;; PGP ;;
;;;;;;;;;

(setopt epg-pinentry-mode 'loopback)
(use-package pinentry :ensure t :config (pinentry-start))

;;
;; Org Mode
;; --------
;;

(unless (package-installed-p 'org)
  (package-vc-install '(org . (:url "https://git.savannah.gnu.org/git/emacs/org-mode.git"))))

;; Defer loading org mode for better startup performance
(with-eval-after-load 'package
  (defvar org-modules '(org-bbdb org-bibtex org-bookmarks org-crypt org-docview org-gnus org-habit org-info org-irc org-mew org-mouse org-noter org-pandoc org-present org-protocol org-rmail org-tempo org-timer))
  (dolist (module org-modules)
    (unless (package-installed-p module)
      (package-vc-install `(,module . (:url "https://git.savannah.gnu.org/git/emacs/org-mode.git")))))
  (defun load-org-module ()
    (require 'org))
  (with-eval-after-load 'org
    (dolist (module org-modules)
      (unless (or (package-installed-p module) (member module package-loaded-list))
  (require module))))
  (add-hook 'emacs-startup-hook 'load-org-module))

;; tree-sitter
(setq treesit-language-source-alist
      `((bash "https://github.com/tree-sitter/tree-sitter-bash")
  (c "https://github.com/tree-sitter/tree-sitter-c")
  (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
  (java "https://github.com/tree-sitter/tree-sitter-java")
  (cuda "https://github.com/theHamsta/tree-sitter-cuda")
  (commonlisp "https://github.com/tree-sitter-grammars/tree-sitter-commonlisp")
  (python "https://github.com/tree-sitter/tree-sitter-python")
  (go "https://github.com/tree-sitter/tree-sitter-go")
  (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
  (yaml "https://github.com/ikatyang/tree-sitter-yaml")
  (starlark "https://github.com/amaanq/tree-sitter-starlark")
  (lua "https://github.com/tree-sitter-grammars/tree-sitter-lua")
  (json "https://github.com/tree-sitter/tree-sitter-json")
  (cmake "https://github.com/uyha/tree-sitter-cmake")
  (make "https://github.com/alemuller/tree-sitter-make")
  (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
  (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
  (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
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
  (verilog "https://github.com/tree-sitter/tree-sitter-verilog")
  (php "https://github.com/tree-sitter/tree-sitter-php")
  (kotlin "https://github.com/fwcd/tree-sitter-kotlin")
  ))


;; Remap common major modes to tree-sitter versions
(add-to-list 'major-mode-remap-alist '(js-json-mode . json-ts-mode))
(add-to-list 'major-mode-remap-alist '(java-mode . java-ts-mode))
(add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
(add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
(add-to-list 'major-mode-remap-alist '(js-mode . js-ts-mode))
(add-to-list 'major-mode-remap-alist '(php-mode . php-ts-mode))


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
(add-to-list 'auto-mode-alist '("\\.swift" . swift-ts-mode))
(add-to-list 'auto-mode-alist '("\\.envrc" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-ts-mode))
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-ts-mode))
(add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-ts-mode))

(defun zelcon/install-tree-sitter-swift ()
  (interactive)
  (let ((parser-path (concat user-emacs-directory (file-name-as-directory "third_party") (file-name-as-directory "tree-sitter-swift"))))
    (async-shell-command (format "cd %s && npm run build && make && cp libtree-sitter-swift.* $HOME/.emacs.d/tree-sitter && git clean -fxd" parser-path) nil nil)))

(defun zelcon/install-tree-sitter-langs ()
  "Install all tree-sitter languages. Typically you only need to run
this once."
  (interactive)
  (mapc #'treesit-install-language-grammar
  (mapcar #'car treesit-language-source-alist))
  (zelcon/install-tree-sitter-swift))

(use-package tree-sitter :ensure t)
(use-package tree-sitter-langs
  :ensure t
  :hook
  ((rust-ts-mode . tree-sitter-hl-mode)
   (c++-ts-mode . tree-sitter-hl-mode)
   (c-ts-mode . tree-sitter-hl-mode)
   (yaml-ts-mode . tree-sitter-hl-mode)
   (python-ts-mode . tree-sitter-hl-mode)
   (java-ts-mode . tree-sitter-hl-mode)
   (swift-ts-mode . tree-sitter-hl-mode)
   (javascript-ts-mode . tree-sitter-hl-mode)
   (typescript-ts-mode . tree-sitter-hl-mode)
   (go-ts-mode . tree-sitter-hl-mode)
   (hcl-mode . tree-sitter-hl-mode)
   (json-ts-mode . tree-sitter-hl-mode)
   (php-ts-mode . tree-sitter-hl-mode))
  :config
  (tree-sitter-require 'cpp)
  (add-to-list 'tree-sitter-major-mode-language-alist '(c++-ts-mode . cpp))
  (tree-sitter-require 'c)
  (add-to-list 'tree-sitter-major-mode-language-alist '(c-ts-mode . c))
  (tree-sitter-require 'rust)
  (add-to-list 'tree-sitter-major-mode-language-alist '(rust-ts-mode . rust))
  (tree-sitter-require 'yaml)
  (add-to-list 'tree-sitter-major-mode-language-alist '(yaml-ts-mode . yaml))
  (tree-sitter-require 'java)
  (add-to-list 'tree-sitter-major-mode-language-alist '(java-ts-mode . java))
  (tree-sitter-require 'python)
  (add-to-list 'tree-sitter-major-mode-language-alist '(python-ts-mode . python))
  (tree-sitter-require 'go)
  (add-to-list 'tree-sitter-major-mode-language-alist '(go-ts-mode . go))
  (tree-sitter-require 'typescript)
  (add-to-list 'tree-sitter-major-mode-language-alist '(typescript-ts-mode . typescript))
  (tree-sitter-require 'javascript)
  (add-to-list 'tree-sitter-major-mode-language-alist '(javascript-ts-mode . javascript))
  (tree-sitter-require 'swift)
  (add-to-list 'tree-sitter-major-mode-language-alist '(swift-ts-mode . swift))
  (tree-sitter-require 'hcl)
  (tree-sitter-require 'json)
  (add-to-list 'tree-sitter-major-mode-language-alist '(json-ts-mode . json))
  (tree-sitter-require 'php)
  (add-to-list 'tree-sitter-major-mode-language-alist '(php-ts-mode . php))
  (tree-sitter-require 'kotlin)
  (add-to-list 'tree-sitter-major-mode-language-alist '(kotlin-ts-mode . kotlin)))

;; Swift
(use-package swift-ts-mode :ensure t)

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

(require 'lsp-mode-config)

;; LLDB
(use-package realgud :ensure t :defer t)
(use-package realgud-lldb :ensure t :defer t)

;; CMake
(use-package cmake-mode :ensure t)

;; Makefile
(with-eval-after-load 'makefile-mode
  (setq-default tab-width 4)
  (indent-tabs-mode 1))



;; YAML
(defun zelcon/set-yaml-tab-width ()
  (setq-default tab-width 2)
  (setq-local indent-bars-spacing-override 2)
  (setq-local indent-bars-spacing 2))
(add-hook 'yaml-ts-mode-hook 'zelcon/set-yaml-tab-width)
(setopt yaml-indent-offset 2)
(use-package yaml-pro :ensure t
  :hook
  ((yaml-mode . yaml-ts-mode)
   (yaml-ts-mode . yaml-pro-ts-mode)
   (yaml-pro-ts-mode . zelcon/set-yaml-tab-width))
  :config
  (setopt yaml-indent-offset 2)
  (add-to-list 'major-mode-remap-alist '(yaml-mode . yaml-ts-mode))
  (setq-default tab-width 2))



;; Verilog
(use-package verilog-ts-mode :ensure t)

;; Julia
(use-package julia-ts-mode :ensure t :disabled t)

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
  )

;; Go
(defun zelcon/go-mode-hook ()
  (setq-local tab-width 4)
  (setq-local indent-tabs-mode t)
  (setq-local go-ts-mode-indent-offset 4))
(add-hook 'go-ts-mode-hook #'zelcon/go-mode-hook)


;;;
;;; Completion
;;; ----------
;;;

(use-package company
  :ensure t
  :config
  ;; disable inline previews
  (delq 'company-preview-if-just-one-frontend company-frontends)
  ;; escape from company completions
  :hook ((prog-mode . company-mode)
   (yaml-ts-mode . company-mode))
  :custom
  (company-idle-delay 0.3))
(use-package company-box
    :ensure t
    :when (not (null window-system))
    :hook (company-mode . company-box-mode))

(use-package s :ensure t)
(use-package dash :ensure t)
(use-package editorconfig :ensure t)
(use-package jsonrpc :ensure t)
(use-package spinner :ensure t)

(use-package ellama
  :ensure t
  :requires spinner
  :init
  (require 'llm-ollama)
  (cl-flet ((mkmodel (model)
                  (make-llm-ollama :host "172.21.21.6" :port 11434 :chat-model model :embedding-model model)))
    (setopt ellama-provider
            (mkmodel "qwen2.5-coder:32b-instruct-q6_K"))
    (setopt ellama-providers
            `(("small" . ,(mkmodel "qwen2.5-coder:0.5b-instruct"))
              ("medium" . ,(mkmodel "qwen2.5-coder:14b-instruct"))
              ("large" . ,(mkmodel "qwen2.5-coder:32b-instruct-q6_K"))
              ("qwq" . ,(mkmodel "qwq:32b-q4_K_M"))
              ("gemma3" . ,(mkmodel "gemma3:27b-it-q8_0")))))
  (setopt ellama-language "English"
          ellama-chat-display-action-function #'display-buffer-full-frame
          ellama-instant-display-action-function #'display-buffer-at-bottom
          ellama-naming-scheme 'ellama-generate-name-by-llm)
  :custom
  (ellama-auto-scroll t)
  :config
  (setopt ellama-keymap-prefix "C-c e"))

;; Yasnippet
(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))
;; install official snippets
(use-package yasnippet-snippets
  :ensure t)

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

;;
;; Code formatting
;; ---------------

(use-package reformatter :ensure t)

(use-package shfmt)

(use-package format-sql)

;;
;; Misc
;; ----

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

;; Insert shell commands to buffer
(defun zelcon/insert-shell-command-output (command)
  "Insert the output of a shell command into the current buffer."
  (interactive "sShell command: ")
  (insert
   (string-trim-right
    (shell-command-to-string command))))
(global-set-key (kbd "C-c !") 'zelcon/insert-shell-command-output)

;;;;;;;;;;;;;;;;;;;;;;;;
;; Random keybindings ;;
;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Appearance
;; ----------
;;

;; theme
(use-package solarized-theme
  :ensure t
  :config
  (load-theme 'solarized-dark t))

;; Set font in GUI mode
(let* ((font-preferences '("JetBrains Mono Nerd Font" "Operator Mono"))
       (font-sizes '(11 17))
       (font-found nil))
  (while (and font-preferences (not font-found))
    (let ((current-font (pop font-preferences))
          (current-size (pop font-sizes)))
      (when (find-font (font-spec :family current-font))
        (set-frame-font (format "%s %d" current-font current-size) nil t)
        (setq font-found t)
        (message "Font set to %s" current-font)))))

;; always highlight current line
(global-hl-line-mode)

;; whitespace-mode
(setopt
 whitespace-style
 '(face trailing tabs spaces empty indentation space-after-tab
  space-before-tab space-mark tab-mark))
(add-hook 'c++-ts-mode-hook 'whitespace-mode)
(add-hook 'yaml-ts-mode-hook 'whitespace-mode)
(add-hook 'python-ts-mode-hook 'whitespace-mode)
(add-hook 'json-ts-mode-hook 'whitespace-mode)
(add-hook 'java-ts-mode-hook 'whitespace-mode)
(add-hook 'lua-ts-mode-hook 'whitespace-mode)
(add-hook 'lisp-mode-hook 'whitespace-mode)
(add-hook 'clojure-ts-mode-hook 'whitespace-mode)
(add-hook 'rust-ts-mode-hook 'whitespace-mode)
(add-hook 'makefile-mode-hook 'whitespace-mode)
(add-hook 'makefile-bsdmake-mode-hook 'whitespace-mode)
(add-hook 'makefile-gmake-mode-hook 'whitespace-mode)
(add-hook 'sql-mode-hook (lambda ()
         (whitespace-mode 1)))

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
(setopt display-line-numbers-type 'relative)

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
    (setopt indent-bars-prefer-character t)
    (custom-set-faces '(whitespace-space ((t (:slant normal))))))
  :hook ((python-ts-mode . indent-bars-mode)
   (yaml-ts-mode . indent-bars-mode)
   (js-json-mode . indent-bars-mode)
   (tsx-ts-mode . indent-bars-mode)
   (json-ts-mode . indent-bars-mode)
   (nxml-mode . indent-bars-mode)
   (java-ts-mode . indent-bars-mode)
   (rust-ts-mode . indent-bars-mode))
  :custom
  (indent-bars-starting-column 0)
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
(global-unset-key (kbd "s-t"))
(global-set-key (kbd "s-t") 'tab-bar-new-tab)
(global-unset-key (kbd "s-w"))
(global-set-key (kbd "s-w") 'tab-bar-close-tab)

;; highlight symbol
(use-package highlight-symbol
  :ensure t
  :custom
  (highlight-symbol-idle-delay 1.5))

;; Transparent background in terminal emulators
(defun on-after-init ()
  (unless (display-graphic-p (selected-frame))
    (set-face-background 'default "unspecified-bg" (selected-frame))))
(add-hook 'window-setup-hook #'on-after-init)
;; [[https://stackoverflow.com/questions/19054228/emacs-disable-theme-background-color-in-terminal/33298750#33298750][Emacs: disable theme background color in terminal - Stack Overflow]]
(defun on-frame-open (&optional frame)
  "If the FRAME created in terminal don't load background color."
  (unless (display-graphic-p frame)
    (set-face-background 'default "unspecified-bg" frame)))
(add-hook 'after-make-frame-functions #'on-frame-open)


(setq package-install-upgrade-built-in t)

(setq load-prefer-newer t)

(load "camelize.el")

(require 'zelcon--util)
(require 'zelcon--ctx-dump)

;; load custom file
(load custom-file t)
