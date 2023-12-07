{ config, pkgs, ... }:
{
  home.username = "zds";
  home.homeDirectory = "/home/zds";

  home.stateVersion = "23.11";

  programs.home-manager.enable = true;

  home.packages = [
    pkgs.htop
    pkgs.tree
    pkgs.fortune
    pkgs.jdt-language-server
    pkgs.dotnet-sdk_8
  ];

  home.file.".lldbinit".source = include/.lldbinit;
  home.file.".gdbinit".source = include/.gdbinit;
  home.file.".emacs.d/third_party" = {
    source = include/emacs/third_party;
  };

  programs.emacs = {
    enable = true;
    extraPackages = epkgs: with epkgs; [
      use-package
      nix-mode
      eglot
      magit
      yaml-mode
    ];
    extraConfig = ''
      ;;; MELPA
      
      (require 'package)
      (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
      ;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
      ;; and `package-pinned-packages`. Most users will not need or want to do this.
      ;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
      (package-initialize)
      
      (let ((default-directory (expand-file-name
      			  (concat user-emacs-directory
      				  "third_party"))))
        (normal-top-level-add-subdirs-to-load-path))
      (eval-when-compile
        (require 'use-package))
      
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
      
      ;;
      ;; Git
      ;; ---
      ;;
      (use-package magit)
      
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
      
      ;;
      ;; LSP
      ;; ---
      ;;
      
      (use-package eglot)
      
      (use-package nix-mode :mode "\\.nix\\'")

      (use-package yaml-mode)
      
      ;;
      ;; Appearance
      ;; ----------
      ;;
      
      ;; theme
      (load-theme 'tsdh-dark)
    '';
  };

  services.gpg-agent = {
    enable = true;
    defaultCacheTtl = 86400;
    enableSshSupport = true;
    defaultCacheTtlSsh = 86400;
  };

  programs.tmux = {
    enable = true;
    terminal = "tmux-256color";
    historyLimit = 100000;
    extraConfig = ''
      unbind C-b
      bind-key C-o set-option -g prefix C-o
      set -g prefix C-o
      
      # https://github.com/neovim/neovim/wiki/FAQ#esc-in-tmux-or-gnu-screen-is-delayed
      set -sg escape-time 10
      set -sg repeat-time 600 # increase repeat timeout
      
      # If keys are entered faster than one in milliseconds, they
      # are assumed to have been pasted rather than typed and tmux
      # key bindings are not processed.  The default is one
      # millisecond and zero disables.
      
      set assume-paste-time 1
    '';
  };

  programs.fzf = {
    enable = true;
    enableZshIntegration = true;
  };

  programs.git = {
    enable = true;
    package = pkgs.gitFull;
    userEmail = "zelcon@zelcon.net";
    userName = "Zelly Snyder";
    signing.key = "D3B05DF8786B7D2C";
    signing.signByDefault = true;
  };

  programs.zsh = {
    enable = true;
    history = {
      path = "${config.xdg.dataHome}/zsh/history";
      extended = true;
      expireDuplicatesFirst = true;
      share = false;
      size = 10000000;
    };
    enableAutosuggestions = true;
    enableCompletion = true;
    defaultKeymap = "emacs";
    initExtra = ''
      # edit command line
      autoload -z edit-command-line
      zle -N edit-command-line
      bindkey "^X^E" edit-command-line
      
      # navigate word boundaries like bash
      autoload -U select-word-style
      select-word-style bash
    '';
  };
}

