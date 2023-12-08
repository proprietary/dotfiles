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

  services.gpg-agent = {
    enable = true;
    defaultCacheTtl = 86400;
    enableSshSupport = true;
    defaultCacheTtlSsh = 86400;
  };

  programs.tmux = {
    enable = true;
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

  programs.vim = {
    enable = true;
    plugins = with pkgs.vimPlugins; [
      vim-nix
      vim-fugitive
      vim-yaml
    ];
    settings = {
      expandtab = true;
      background = "dark";
      tabstop = 4;
      shiftwidth = 4;
      copyindent = true;
    };
  };

  programs.fzf = {
    enable = true;
    enableZshIntegration = true;
  };

  programs.git = {
    enable = true;
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

      setopt autocd # so I may omit "cd" to change dirs

      export EDITOR=emacs
    '';
  };
}

