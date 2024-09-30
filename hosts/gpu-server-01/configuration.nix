# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, lib, ... }:

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
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      ./sops.nix
      ./zelcon.net-vpn.nix
      ./flamingo-vpn.nix
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

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    home-manager

    emacs
    jansson
    wget
    tree-sitter
    tmux
    curl
    wireguard-tools
    gnupg
    gitFull
    git-crypt
    openssl
    nmap
    zfs

    jdk21
    python312Full
    ruby
    julia
    R
    postgresql
    go
    rustup
    sbcl

    docker
    kubectl
    skaffold
    kubernetes-helm
    kubernetes

    gcc
    gdb
    libgcc
    libgccjit
    clang_18
    lldb_18
    llvm_18
    libcxx
    libunwind
    bazel
    cmake
    pkg-config
    gnumake
    ninja
    autoconf
    bear
    gnum4
    rr

    cudatoolkit
    nvidia-docker
    vulkan-tools

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

    boost
    python311Packages.boost
    python311Packages.numpy
    folly
    ffmpeg
    opencv

    grafana
    prometheus
    valgrind
    fzf
    sqlite
    sshfs
    nfs-utils
    ripgrep
    nix-index
    coreutils-full
    patchelf
    aspell
    aspellDicts.en
    mosh
    zstd
    ollama
    xsel
    vim
    neovim
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
    gnused
    gnutar
    gawk

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
  services.openssh.enable = true;
  services.openssh.extraConfig = ''
    TrustedUserCAKeys /run/secrets/net_zelcon/ssh_CA_pub
  '';

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  networking.firewall.enable = false;

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

  systemd.services."systemd-networkd".environment.SYSTEMD_LOG_LEVEL = "debug";

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
  };


  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "23.05"; # Did you read the comment?

}
