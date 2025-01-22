;; lsp generates a lot of garbage
(setopt gc-cons-threshold 100000000)

(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook ((c++-ts-mode . lsp)
         (rust-ts-mode . lsp)
         (java-ts-mode . lsp)
         (python-ts-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(use-package lsp-ui :commands lsp-ui-mode :ensure t)

(use-package dap-mode :ensure t
  :config
  (dap-ui-mode)
  (dap-ui-controls-mode 1)
  (use-package dap-lldb :ensure t)
  (use-package dap-gdb-lldb :ensure t)
  (use-package dap-java :ensure t)
  (use-package dap-python)
  (use-package dap-dlv-go :ensure t))


(provide 'lsp-mode-config)
