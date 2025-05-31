{
  pkgs,
  config,
  ...
}:
{
  home.username = "zds";
  home.homeDirectory = "/home/zds";

  home.stateVersion = "23.11";

  programs.home-manager.enable = true;

  home.packages = with pkgs; [
    htop
    iftop
    grepcidr
    jq
    yq-go
    emacs-lsp-booster
  ];

  home.file.".lldbinit".source = include/.lldbinit;
  home.file.".gdbinit".source = include/.gdbinit;
  home.file.".tmux.conf".source = include/.tmux.conf;
  #home.file.".tmux".source = include/.tmux;

  programs.starship = {
    enable = true;
    enableZshIntegration = true;
    settings = {
      time.disabled = false;
      time.time_format = "%a %e-%b-%Y %I:%M:%S%p";
      aws.disabled = true;
      gcloud.disabled = true;
    };
  };

  programs.direnv = {
    enable = true;
    enableZshIntegration = true;
  };

  services.gpg-agent = {
    enable = true;
    enableSshSupport = true;
    defaultCacheTtl = 86400;
    defaultCacheTtlSsh = 86400;
    enableZshIntegration = true;
    pinentry.package = pkgs.pinentry-emacs;
    extraConfig = ''
      allow-emacs-pinentry
      allow-loopback-pinentry
    '';
  };

  programs.vim = {
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
    lfs = {
      enable = true;
    };
    extraConfig = {
      core = {
        excludesFile = let global_gitignore = pkgs.writeText "${config.home.homeDirectory}/.gitignore" ''
        .DS_Store
        .envrc
        .envrc.local
        .env
        Thumbs.db
        desktop.ini
        node_modules/
        .vscode/
        .idea/
        .cache/
        .ccls-cache/
        .clangd/
        .mypy_cache/
        .pytest_cache/
        .tox/
        .venv/
        .vscode
        '';
        in
        "${config.home.homeDirectory}/.gitignore";
      };
    };
  };

  programs.zsh = {
    enable = true;
    history = {
      path = "${config.xdg.dataHome}/zsh/history";
      extended = true;
      expireDuplicatesFirst = true;
      share = true;
      save = 10737418240;
      size = 10737418240;
      #append = true;
      #historySubstringSearch.enable = true;
      ignoreSpace = true;
      ignorePatterns = [
        "rm *"
        "pkill *"
        "kill *"
        "dd *"
        "gpg *"
        "sshpass *"
      ];
    };
    autosuggestion.enable = true;
    enableCompletion = true;
    defaultKeymap = "emacs";
    autocd = true;
    initContent = ''
      # edit command line
      autoload -z edit-command-line
      zle -N edit-command-line
      bindkey "^X^E" edit-command-line

      # navigate word boundaries like bash
      autoload -U select-word-style
      select-word-style bash

      typeset -U path cdpath fpath manpath

      export EDITOR=emacs
      export VISUAL=emacs
      export PAGER=less
      export LESS="-R"

      alias ls='ls --color --group-directories-first --time-style=long-iso --human-readable -la'
      alias emacs='emacs -nw'

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

      eval "$(direnv hook zsh)"
    '';
  };
}
