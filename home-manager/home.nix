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
    pkgs.starship
  ];

  home.file.".lldbinit".source = include/.lldbinit;
  home.file.".gdbinit".source = include/.gdbinit;
  home.file.".tmux.conf".source = include/.tmux.conf;
  home.file.".config/starship.toml".source = include/starship.toml;

  services.gpg-agent = {
    enable = true;
    defaultCacheTtl = 86400;
    enableSshSupport = true;
    defaultCacheTtlSsh = 86400;
  };

  programs.tmux.enable = true;

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

      export GPG_TTY=$(tty)

      zstyle ':completion:*' completer _expand _complete _ignored _correct _approximate
      autoload -Uz compinit bashcompinit
      compinit
      bashcompinit

      autoload -Uz vcs_info
      zstyle ':vcs_info:*' enable git
      precmd() { vcs_info }
      setopt prompt_subst
      zstyle ':vcs_info:git*' formats "%{$fg[grey]%}%s %{$reset_color%}%r/%S%{$fg[grey]%} %{$fg[blue]%}%b%{$reset_color%}%m%u%c%{$reset_color%} "

      eval "$(starship init zsh)"

      eval "$(direnv hook zsh)"
    '';
  };
}

