;; lsp generates a lot of garbage
(setopt gc-cons-threshold 100000000)

(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook ((c++-ts-mode . lsp)
         (c-ts-mode . lsp)
         (rust-ts-mode . lsp)
         (java-ts-mode . lsp)
         (python-ts-mode . lsp)
         (nix-ts-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(use-package lsp-ui :commands lsp-ui-mode :ensure t)

(use-package dap-mode :ensure t
  :config
  (dap-ui-mode 1)
  (dap-ui-controls-mode 1))
(use-package lsp-java :ensure t
  :config
  (defun lsp-java--ls-command ()
    '("jdtls"
      "-configuration" "../config-linux"
      "-data" "../java-workspace"))
  (require 'dap-java))
(require 'dap-lldb)
(require 'dap-gdb-lldb)
(require 'dap-python)
(setq dap-python-debugger 'debugpy)
(require 'dap-dlv-go)

(provide 'lsp-mode-config)
