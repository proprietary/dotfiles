{
  config,
  pkgs,
  lib,
  unstable,
  ...
}:
{
  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix
    ./sops.nix
    ./smart-home.nix
    ./zelcon.net-vpn.nix
    ./clickhouse.nix
    ./virtualization.nix
    ./webshit.nix
  ];

  # Bootloader.
  boot.loader.grub.enable = true;
  boot.loader.grub.device = "/dev/sdb";
  boot.loader.grub.useOSProber = true;
  boot.loader.grub.memtest86.enable = true; # Make memtest86+ available from the GRUB boot menu

  networking.hostName = "superstorage"; # Define your hostname.
  networking.hostId = "6c37c393";

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Enable magic SysRq key
  boot.kernel.sysctl."kernel.sysrq" = 1;

  # Set your time zone.
  time.timeZone = "America/Los_Angeles";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";

  i18n.extraLocaleSettings = {
    LC_ADDRESS = "en_US.UTF-8";
    LC_IDENTIFICATION = "en_US.UTF-8";
    LC_MEASUREMENT = "en_US.UTF-8";
    LC_MONETARY = "en_US.UTF-8";
    LC_NAME = "en_US.UTF-8";
    LC_NUMERIC = "en_US.UTF-8";
    LC_PAPER = "en_US.UTF-8";
    LC_TELEPHONE = "en_US.UTF-8";
    LC_TIME = "en_US.UTF-8";
  };

  # Configure keymap in X11
  services.xserver.xkb = {
    layout = "us";
    variant = "";
  };

  # ZFS
  boot.zfs.package = pkgs.zfs_unstable;
  boot.kernelPackages =
    let
      zfsCompatibleKernelPackages = lib.filterAttrs (
        name: kernelPackages:
        (builtins.match "linux_[0-9]+_[0-9]+" name) != null
        && (builtins.tryEval kernelPackages).success
        && (!kernelPackages.${config.boot.zfs.package.kernelModuleAttribute}.meta.broken)
      ) pkgs.linuxKernel.packages;
    in
    lib.last (
      lib.sort (a: b: (lib.versionOlder a.kernel.version b.kernel.version)) (
        builtins.attrValues zfsCompatibleKernelPackages
      )
    );
  boot.supportedFilesystems = [ "zfs" ];
  boot.zfs.forceImportRoot = false;
  services.nfs.server.enable = true;
  boot.zfs.extraPools = [ "superstorage" ];
  services.zfs.autoScrub.enable = true;
  boot.kernelParams = [ "nohibernate" ];

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.zds = {
    isNormalUser = true;
    description = "zds";
    extraGroups = [
      "networkmanager"
      "wheel"
    ];
    packages = with pkgs; [ ];
  };
  security.sudo.wheelNeedsPassword = false;
  users.defaultUserShell = pkgs.zsh;
  programs.zsh.enable = true;
  programs.direnv.enable = true;
  # get zsh completions for system packages (e.g., systemd)
  environment.pathsToLink = [ "/share/zsh" ];

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  nix.settings.experimental-features = [
    "nix-command"
    "flakes"
  ];
  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    home-manager
    nix-index
    nixfmt-rfc-style
    emacs-gtk
    jansson
    wget
    tree-sitter
    tmux
    wireguard-tools
    gnupg
    pinentry-all
    gitFull
    git-crypt
    openssl
    nmap
    zfs_unstable
    btrfs-progs
    ffmpeg
    mplayer
    google-authenticator
    sops
    wget
    neovim
    curl
    iproute2
    mosh
    mtr
    iperf3
    dnsutils
    ldns
    aria2
    socat
    nmap
    ipcalc
    file
    which
    tree
    jq
    yq
    libxml2
    folly
    boost
    ollama
    binutils
    p7zip

    # Compilers etc.
    jdk23
    python313Full
    python313Packages.debugpy
    julia
    R
    postgresql
    go
    rustup
    sbcl
    glibc
    gcc_multi
    gdb
    libgcc
    libgccjit
    lldb
    llvm-manpages
    include-what-you-use
    clang-tools
    cppcheck
    bazelisk
    cmake
    pkg-config
    gnumake
    ninja
    autoconf
    bear
    gnum4
    rr
    valgrind
    yasm
    sqlite
    nfs-utils
    ripgrep
    coreutils-full
    patchelf
    aspell
    aspellDicts.en
    swift
    nodejs_22
    shellcheck
    maven
    lua
    php
    terraform
    asmfmt
    global
    universal-ctags
    pkg-config
    libiconv
    xclip
    xsel
    vscode
    jetbrains.idea-community-bin
    brave
    ghostty
    turbovnc

    # LSPs
    jdt-language-server
    gopls
    nil
    pyright
    cmake-language-server
    yaml-language-server
    phpactor
    sourcekit-lsp
    terraform-ls
    javascript-typescript-langserver
    dockerfile-language-server-nodejs
    ansible-language-server
    # tree-sitter grammars
    tree-sitter-grammars.tree-sitter-cpp
    tree-sitter-grammars.tree-sitter-c
    tree-sitter-grammars.tree-sitter-rust
    tree-sitter-grammars.tree-sitter-cuda
    tree-sitter-grammars.tree-sitter-yaml
    tree-sitter-grammars.tree-sitter-cmake
    tree-sitter-grammars.tree-sitter-proto
    tree-sitter-grammars.tree-sitter-bash
    tree-sitter-grammars.tree-sitter-go
    tree-sitter-grammars.tree-sitter-gomod
    tree-sitter-grammars.tree-sitter-commonlisp
    tree-sitter-grammars.tree-sitter-dockerfile
    tree-sitter-grammars.tree-sitter-hcl
    tree-sitter-grammars.tree-sitter-toml
    tree-sitter-grammars.tree-sitter-sql
    tree-sitter-grammars.tree-sitter-ruby
    tree-sitter-grammars.tree-sitter-r
    tree-sitter-grammars.tree-sitter-python
    tree-sitter-grammars.tree-sitter-perl
    tree-sitter-grammars.tree-sitter-nix
    tree-sitter-grammars.tree-sitter-make
    tree-sitter-grammars.tree-sitter-lua
    tree-sitter-grammars.tree-sitter-latex
    tree-sitter-grammars.tree-sitter-llvm
    tree-sitter-grammars.tree-sitter-json
    tree-sitter-grammars.tree-sitter-typescript
    tree-sitter-grammars.tree-sitter-tsx
    tree-sitter-grammars.tree-sitter-javascript
    tree-sitter-grammars.tree-sitter-jsdoc
    tree-sitter-grammars.tree-sitter-html
    tree-sitter-grammars.tree-sitter-elisp
    tree-sitter-grammars.tree-sitter-php

    # Ruby
    ruby_3_3
    rubyPackages_3_3.ruby-lsp
    rubyPackages_3_3.stringio
    rubyPackages_3_3.rbs
    rubyPackages_3_3.sorbet-runtime
    rubyPackages_3_3.prism
    rubyPackages_3_3.ffi
    rubyPackages_3_3.psych

    # Archives
    gnused
    gnutar
    gawk
    zip
    xz
    unzip
    p7zip
    zstd

    # system call monitoring
    strace
    ltrace
    lsof

    # system tools
    wakelan
    ethtool
    nmap
    sysstat
    lm_sensors
    pciutils # lspci
    usbutils # lsusb

    # diagnostics
    memtester
    hdparm
    smartmontools
  ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh = {
    enable = true;
    openFirewall = true;
    sftpFlags = [
      # Log sftp level file access (read/write/etc.) that would not be easily logged otherwise.
      "-f AUTHPRIV"
      "-l INFO"
    ];
    hostKeys = [
      {
        path = "/etc/ssh/ssh_host_ed25519_key";
        type = "ed25519";
      }
      {
        bits = 4096;
        path = "/etc/ssh/ssh_host_rsa_key";
        type = "rsa";
      }
    ];
    settings = {
      X11Forwarding = true;

      # Hardening
      Protocol = "2";
      PermitRootLogin = "no";
      # https://infosec.mozilla.org/guidelines/openssh#modern-openssh-67
      KexAlgorithms = [
        "curve25519-sha256@libssh.org,ecdh-sha2-nistp521"
        "ecdh-sha2-nistp384"
        "ecdh-sha2-nistp256"
        "diffie-hellman-group-exchange-sha256"
      ];
      Ciphers = [
        "chacha20-poly1305@openssh.com"
        "aes256-gcm@openssh.com"
        "aes128-gcm@openssh.com"
        "aes256-ctr"
        "aes192-ctr"
        "aes128-ctr"
      ];
      Macs = [
        "hmac-sha2-512-etm@openssh.com"
        "hmac-sha2-256-etm@openssh.com"
        "umac-128-etm@openssh.com"
        "hmac-sha2-512"
        "hmac-sha2-256"
        "umac-128@openssh.com"
      ];

      AuthenticationMethods = "publickey";

      # LogLevel VERBOSE logs user's key fingerprint on login. Needed to have a clear audit track of which key was using to log in.
      LogLevel = "VERBOSE";

      # Certificates
      TrustedUserCAKeys = "/run/secrets/net_zelcon/ssh_CA_pub";
    };
  };

  programs.mosh = {
    enable = true;
    openFirewall = true;
  };

  networking.firewall = {
    enable = true;
  };

  networking.networkmanager.enable = false;
  networking.useNetworkd = true;
  boot.kernel.sysctl."net.ipv4.ip_forward" = "1";
  boot.kernel.sysctl."net.ipv6.conf.all.forwarding" = "1";
  systemd.network = {
    enable = true;
    networks."ethernet" = {
      matchConfig.Name = "en*";
      networkConfig = {
        DHCP = "yes";
        IPv4Forwarding = "yes";
        IPv6Forwarding = "yes";
        IPMasquerade = "both";
        IPv6PrivacyExtensions = "kernel";
      };
    };
  };
  networking.interfaces."enp*s*".wakeOnLan = {
    enable = true;
    policy = [
      "magic"
      "broadcast"
    ];
  };

  programs.nix-ld = {
    enable = true;
    libraries = with pkgs; [ zlib ];
  };

  services.ollama.enable = true;

  #######
  # GUI #
  #######

  # Software rendering for headless server
  hardware.graphics.enable = true;

  services.xserver.enable = true;

  # services.xserver.desktopManager.gnome.enable = true;
  # services.displayManager.gdm.enable = true;

  services.desktopManager.plasma6.enable = true;
  services.displayManager.defaultSession = "plasmax11";
  services.displayManager.sddm.enable = true;

  fonts.packages = with pkgs; [
    nerd-fonts.jetbrains-mono
  ];

  # Remoting
  services.xrdp = {
    enable = true;
    openFirewall = false;
    defaultWindowManager = "${pkgs.kdePackages.plasma-workspace}/bin/startplasma-x11";
  };
  users.users.xrdp = {
    isSystemUser = true;
    group = "xrdp";
    extraGroups = [ "video" "input" ];
  };
  # services.rustdesk-server = {
  #   enable = true;
  #   openFirewall = true;
  #   signal = {
  #     enable = true;
  #     relayHosts = ["172.21.21.1:21117"];
  #     extraArgs = [
  #       "--mask" "172.21.21.0/24"
  #       "-M" "33554432"
  #     ];
  #   };
  #   relay = {
  #     enable = true;
  #   };
  # };
  # systemd.services.rustdesk-signal.environment.ALWAYS_USE_RELAY = "Y";

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "24.05"; # Did you read the comment?

}
