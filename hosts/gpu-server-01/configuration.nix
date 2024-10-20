# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, lib, unstable, ... }:

let
  isUnstable = config.boot.zfs.package == pkgs.zfsUnstable;
  zfsCompatibleKernelPackages = lib.filterAttrs (
    name: kernelPackages:
    (builtins.match "linux_[0-9]+_[0-9]+" name) != null
    && (builtins.tryEval kernelPackages).success
    && (
      (!isUnstable && !kernelPackages.zfs.meta.broken)
      || (isUnstable && !kernelPackages.zfs_unstable.meta.broken)
    )
  ) pkgs.linuxKernel.packages;
  latestKernelPackage = lib.last (
    lib.sort (a: b: (lib.versionOlder a.kernel.version b.kernel.version)) (
      builtins.attrValues zfsCompatibleKernelPackages
    )
  );
in
{
  imports = [
      ./hardware-configuration.nix
      ./sops.nix
      ./zelcon.net-vpn.nix
      ./flamingo-vpn.nix
      ./bind.nix
    ];

  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "gpu-server-01"; # Define your hostname.
  networking.hostId = "409af881";
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Enable networking
  networking.networkmanager.enable = false;
  networking.useNetworkd = true;
  systemd.network.enable = true;

  # Enable IP forwarding
  boot.kernel.sysctl."net.ipv4.ip_forward" = "1";
  boot.kernel.sysctl."net.ipv6.conf.all.forwarding" = "1";

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

  # ZFS
  boot.kernelPackages = latestKernelPackage;
  boot.supportedFilesystems = [ "zfs" ];
  boot.zfs.forceImportRoot = false;
  services.nfs.server.enable = true;

  # Configure keymap in X11
  services.xserver.xkb = {
    layout = "us";
    variant = "";
  };

  programs.zsh.enable = true;

  programs.direnv.enable = true;

  security.sudo.wheelNeedsPassword = false;

  users.defaultUserShell = pkgs.zsh;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.zds.isNormalUser = true;
  users.users.zds.extraGroups = [ "wheel" "libvirtd" ];

  # get zsh completions for system packages (e.g., systemd)
  environment.pathsToLink = [ "/share/zsh" ];

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.nvidia.acceptLicense = true;

  nixpkgs.overlays = [
    (final: prev: {
      ollama = unstable.ollama;
      emacs = unstable.emacs30;
    })
  ];

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    home-manager
    nix-index
    nixfmt-rfc-style

    emacs
    jansson
    wget
    tree-sitter
    tmux
    curl
    wireguard-tools
    gnupg
    pinentry
    gitFull
    git-crypt
    openssl
    nmap
    zfs
    google-authenticator

    jdk21
    python312Full
    ruby
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
    unstable.llvmPackages_19.stdenv
    unstable.llvmPackages_19.clang-unwrapped
    unstable.llvmPackages_19.libcxx
    unstable.llvmPackages_19.bintools
    unstable.llvmPackages_19.openmp
    unstable.llvmPackages_19.libunwind
    unstable.llvmPackages_19.llvm-manpages
    unstable.llvmPackages_19.lldb-manpages
    unstable.llvmPackages_19.clang-manpages
    bazel
    cmake
    pkg-config
    gnumake
    ninja
    autoconf
    bear
    gnum4
    rr
    valgrind
    sqlite
    nfs-utils
    ripgrep
    coreutils-full
    patchelf
    aspell
    aspellDicts.en
    vim
    neovim
    boost
    python311Packages.boost
    python311Packages.numpy
    folly
    ffmpeg
    opencv
    lua
    nodejs_22
    nodePackages.pyright
    jdt-language-server
    yaml-language-server
    nixd
    gopls
    lemminx
    sqls
    clojure-lsp
    lua-language-server
    nodePackages.vscode-json-languageserver
    nodePackages.bash-language-server

    # GPU
    cudaPackages.cudatoolkit
    #cudaPackages.tensorrt
    cudaPackages.cudnn
    cudaPackages.cuda_gdb
    cudaPackages.libnpp
    cudaPackages.cutensor
    cudaPackages.nvidia_fs
    cudaPackages.libcurand
    cudaPackages.libcublas
    cudaPackages.libcufile
    cudaPackages.cuda_nvcc
    cudaPackages.cuda_nvtx
    nvidia-docker
    vulkan-tools
    ollama

    # Containers
    docker
    kubectl
    skaffold
    kubernetes-helm
    kubernetes

    grafana
    prometheus

    sshfs
    xsel

    # Networking
    mtr # A network diagnostic tool
    iperf3
    dnsutils  # `dig` + `nslookup`
    ldns # replacement of `dig`, it provide the command `drill`
    aria2 # A lightweight multi-protocol & multi-source command-line download utility
    socat # replacement of openbsd-netcat
    nmap # A utility for network discovery and security auditing
    ipcalc  # it is a calculator for the IPv4/v6 addresses
    file
    which
    tree

    # Archives
    gnused
    gnutar
    gawk
    zip
    xz
    unzip
    p7zip
    zstd

    # VMs
    spice
    spice-gtk
    spice-protocol
    virt-viewer
    virtio-win
    win-spice

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
  ];

  fonts.packages = with pkgs; [
    font-awesome_5
    nerdfonts
    fira-code
    whatsapp-emoji-font
    go-font
  ];


  nix.settings.experimental-features = [ "nix-command" "flakes" ];


  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
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

      # LogLevel VERBOSE logs user's key fingerprint on login. Needed to have a clear audit track of which key was using to log in.
      LogLevel = "VERBOSE";

      # Google Authenticator
      UsePAM = true;
      AuthenticationMethods = "publickey,keyboard-interactive";
      PermitEmptyPasswords = "no";

      # Certificates
      TrustedUserCAKeys = "/run/secrets/net_zelcon/ssh_CA_pub";
    };
  };
  security.pam.services.sshd.text = ''
    account required pam_unix.so # unix (order 10900)

    # Google Authenticator
    auth required ${pkgs.google-authenticator}/lib/security/pam_google_authenticator.so nullok no_increment_hotp # google_authenticator (order 12500)
    # Allow users without a ~/.google_authenticator
    auth sufficient pam_permit.so

    session required pam_env.so conffile=/etc/pam/environment readenv=0 # env (order 10100)
    session required pam_unix.so # unix (order 10200)
    session required pam_loginuid.so # loginuid (order 10300)
    session optional ${pkgs.systemd}/lib/security/pam_systemd.so # systemd (order 12000)
  '';

  programs.mosh = {
    enable = true;
    openFirewall = true;
  };

  # Firewall
  networking.firewall = {
    enable = true;
    allowedTCPPorts = [
      22
    ];
    allowedUDPPorts = [
      63618 # wg0 / zelcon.net
      63636 # wg1 / flamingo
    ];
    trustedInterfaces = [
      "wg0"
      "wg1"
    ];
  };

  # Wake On Lan
  networking.interfaces."enp*s*".wakeOnLan = {
      enable = true;
      policy = ["magic" "broadcast"];
  };

  # WireGuard
  systemd.network = {
    networks."ethernet" = {
      matchConfig.Name = "enp*s*";
      networkConfig = {
    DHCP = "yes";
    IPForward = "yes";
    IPMasquerade = "both"; };
    };
  };

  #systemd.services."systemd-networkd".environment.SYSTEMD_LOG_LEVEL = "debug";


  programs.virt-manager.enable = true;
  virtualisation = {
    libvirtd = {
      enable = true;
      qemu = {
    package = pkgs.qemu_kvm;
    swtpm.enable = true;
    ovmf.enable = true;
    ovmf.packages = [ pkgs.OVMFFull.fd ];
      };
    };
    spiceUSBRedirection.enable = true;
  };
  systemd.services.libvirtd.enable = true;

  #
  # NVIDIA RTX 4090
  #

  # Enable OpenGL
  hardware.opengl = {
    enable = true;
    driSupport = true;
    driSupport32Bit = true;
  };

  # Load nvidia driver for Xorg and Wayland
  services.xserver.videoDrivers = ["nvidia"];

  hardware.nvidia = {

    # Modesetting is required.
    modesetting.enable = true;

    # Nvidia power management. Experimental, and can cause sleep/suspend to fail.
    powerManagement.enable = false;
    # Fine-grained power management. Turns off GPU when not in use.
    # Experimental and only works on modern Nvidia GPUs (Turing or newer).
    powerManagement.finegrained = false;

    # Use the NVidia open source kernel module (not to be confused with the
    # independent third-party "nouveau" open source driver).
    # Support is limited to the Turing and later architectures. Full list of
    # supported GPUs is at:
    # https://github.com/NVIDIA/open-gpu-kernel-modules#compatible-gpus
    # Only available from driver 515.43.04+
    # Currently alpha-quality/buggy, so false is currently the recommended setting.
    open = false;

    # Enable the Nvidia settings menu,
      # accessible via `nvidia-settings`.
    nvidiaSettings = false;

    # Optionally, you may need to select the appropriate driver version for your specific GPU.
    package = config.boot.kernelPackages.nvidiaPackages.mkDriver {
        #version = "555.58.02";
        version = "560.31.02";
        sha256_64bit = "sha256-0cwgejoFsefl2M6jdWZC+CKc58CqOXDjSi4saVPNKY0=";
        sha256_aarch64 = "sha256-0cwgejoFsefl2M6jdWZC+CKc58CqOXDjSi4saVPNKY0=";
        settingsSha256 = "sha256-A3SzGAW4vR2uxT1Cv+Pn+Sbm9lLF5a/DGzlnPhxVvmE=";
        openSha256 = lib.fakeSha256;
        persistencedSha256 = "sha256-A3SzGAW4vR2uxT1Cv+Pn+Sbm9lLF5a/DGzlnPhxVvmE=";
    };
  };

  services.ollama = {
    enable = true;
    acceleration = "cuda";
    listenAddress = "0.0.0.0:11434";
  };


  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "23.05"; # Did you read the comment?

}
