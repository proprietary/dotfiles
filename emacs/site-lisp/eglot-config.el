(require 'eglot)

(add-to-list 'warning-suppress-types '(eglot eglot--server-stderr))
(add-to-list 'warning-suppress-log-types '(eglot eglot--server-stderr))

(add-hook 'python-ts-mode-hook 'eglot-ensure)
(add-hook 'java-ts-mode-hook 'eglot-ensure)
(add-hook 'c-ts-mode-hook 'eglot-ensure)
(add-hook 'c++-ts-mode-hook 'eglot-ensure)
(add-hook 'go-ts-mode-hook 'eglot-ensure)
(add-hook 'lua-ts-mode-hook 'eglot-ensure)
(add-hook 'javascript-ts-mode-hook 'eglot-ensure)
(add-hook 'typescript-ts-mode-hook 'eglot-ensure)
(add-hook 'rust-ts-mode-hook 'eglot-ensure)
(add-hook 'ruby-ts-mode-hook 'eglot-ensure)
(add-hook 'r-ts-mode-hook 'eglot-ensure)
(add-hook 'julia-ts-mode-hook 'eglot-ensure)
(add-hook 'go-ts-mode 'eglot-ensure)

(define-key eglot-mode-map (kbd "C-c C-a") 'eglot-code-actions)

(evil-define-key 'normal eglot-mode-map (kbd "SPC r") 'eglot-rename)
(evil-define-key 'normal eglot-mode-map (kbd "SPC f") 'eglot-format-buffer)
(evil-define-key 'normal eglot-mode-map (kbd "SPC a") 'eglot-code-actions)

(evil-define-key 'normal eglot-mode-map (kbd "SPC <down>") 'flymake-goto-next-error)
(evil-define-key 'normal eglot-mode-map (kbd "<f9>") 'flymake-goto-next-error)
(evil-define-key 'normal eglot-mode-map (kbd "SPC <up>") 'flymake-goto-prev-error)
(evil-define-key 'normal eglot-mode-map (kbd "<f8>") 'flymake-goto-prev-error)

(defun zelcon/clear-eglot-server-program (mode-name)
  (setq eglot-server-programs
  (assoc-delete-all mode-name
        eglot-server-programs
        (lambda (alist-key our-key)
          (if (listp alist-key)
        (member our-key alist-key)
            (equal our-key alist-key))))))

(use-package eglot-booster
  :init
  (unless (package-installed-p 'eglot-booster)
    (package-vc-install "https://github.com/jdtsmith/eglot-booster.git"))
  :after eglot
  :config (eglot-booster-mode))

;; lsp generates a lot of garbage
(setopt gc-cons-threshold 100000000)

;; Rust
;; Define a setup function that runs in the mode hook.
(defun setup-rust ()
  "Setup for ‘rust-mode’."
  ;; Configuration taken from rust-analyzer’s manual:
  ;; https://rust-analyzer.github.io/manual.html#configuration
  (setq-local eglot-workspace-configuration
              ;; Setting the workspace configuration for every
              ;; rust-mode buffer, you can also set it with dir-local
              ;; variables, should you want different configuration
              ;; per project/directory.
              '(:rust-analyzer
                (:check (:command "clippy")
                        :procMacro (:attributes (:enable t) :enable t)
                        :cargo (:features "all")
                        :diagnostics (:disabled ["unresolved-proc-macro"
                                                 "unresolved-macro-call"])))))

;; Run our setup function in ‘rust-mode-hook’.
(add-hook 'rust-ts-mode-hook #'setup-rust)

;; Define a custom eglot LSP server for rust-analyzer because it
;; expects initializationOptions done a bit differently (see below).
(defclass eglot-rust-analyzer (eglot-lsp-server) ()
  :documentation "A custom class for rust-analyzer.")

;; Rust-analyzer requires the workspaceConfiguration sent as
;; initializationOptions at startup time. See
;; https://github.com/joaotavora/eglot/discussions/845 and
;; rust-analyzer’s manual page.
(cl-defmethod eglot-initialization-options ((server eglot-rust-analyzer))
  eglot-workspace-configuration)

;; Use our custom ‘eglot-rust-analyzer’ for ‘rust-mode’.
(add-to-list 'eglot-server-programs
       '(rust-ts-mode . (eglot-rust-analyzer "rust-analyzer")))
;; PHP
(add-to-list 'eglot-server-programs
       '((php-mode php-ts-mode) .
         ("phpactor" "language-server")))
;; Nix
(use-package nix-ts-mode :ensure t :mode "\\.nix\\'"
  :hook ((nix-ts-mode . eglot-ensure))
  :config
  (add-to-list 'eglot-server-programs
         '(nix-ts-mode . ("nil"))))
;; Terraform
(use-package terraform-mode :ensure t
  :config (add-to-list 'eglot-server-programs `((terraform-mode terraform-ts-mode) . ("terraform-ls" "serve"))))

;; Ansible
(use-package ansible
  :ensure t
  :init
  ;; Add ansible file patterns to auto-mode-alist
  (add-to-list 'auto-mode-alist '("\\.ansible.yml\\'" . ansible-mode))
  (add-to-list 'auto-mode-alist '("\\.ansible.yaml\\'" . ansible-mode))
  :config
  (add-to-list 'eglot-server-programs `((ansible-mode) . '("ansible-language-server" "--stdio")))
  :hook
  ((ansible-mode . eglot-ensure)))
(use-package ansible-doc :ensure t)

;; Ruby
(add-to-list 'eglot-server-programs
       '((ruby-mode ruby-ts-mode) .
         ("ruby-lsp")))

(use-package ggtags
  :ensure t)

(setopt xref-backend-functions
  '(eglot-xref-backend
    ggtags-xref-backend
    etags--xref-backend
    t))

(provide 'eglot-config)
