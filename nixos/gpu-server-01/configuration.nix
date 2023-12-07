# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      <home-manager/nixos>
    ];

  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "nixos"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Enable networking
  networking.networkmanager.enable = true;

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
  services.xserver = {
    layout = "us";
    xkbVariant = "";
  };

  security.sudo.wheelNeedsPassword = false;

  programs.zsh.enable = true;
  users.defaultUserShell = pkgs.zsh;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.zds = {
    isNormalUser = true;
    description = "zds";
    extraGroups = [ "networkmanager" "wheel" ];
    shell = pkgs.zsh;
    packages = with pkgs; [];
  };

  home-manager.users.zds =  { pkgs, ... }: {
    home.packages = [
      pkgs.httpie
      pkgs.atool
    ];
    home.stateVersion = "23.11";
  };

  # get zsh completions for system packages (e.g., systemd)
  environment.pathsToLink = [ "/share/zsh" ];

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    vim
    wget
    emacs
    tmux
    curl
    wireguard-tools
    gnupg
    gitFull
    jdk
    python3
    ruby
    julia
    R
    postgresql
    go
    gcc
    gdb
    docker
    llvmPackages.clangUseLLVM
    llvmPackages.openmp
    llvmPackages.lldb-manpages
    llvmPackages.libunwind
    llvmPackages.compiler-rt
    llvmPackages.compiler-rt-libc
    llvmPackages.libclc
    llvmPackages.bintools
    llvmPackages.libcxxClang
    clang-tools
    bazel
    cmake
    pkg-config
    gnumake
    ninja
    autoconf
    boost
    folly
    ffmpeg
    opencv
    grafana
    prometheus
    cudaPackages.cudatoolkit
    rustup
    sbcl
    valgrind
    fzf
    sqlite
    sshfs
    nfs-utils
    ripgrep
    coreutils-full
    openssl
    patchelf
    aspell
    aspellDicts.en
  ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;
  services.openssh.extraConfig = ''
    TrustedUserCAKeys /var/net_zelcon_CA.pub
  '';

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  networking.firewall.enable = false;

  # WireGuard
  systemd.network = {
    enable = true;
    netdevs = {
      "50-wg0" = {
        netdevConfig = {
          Kind = "wireguard";
          Name = "wg0";
        };
        wireguardConfig = {
          PrivateKeyFile = "/var/wireguard-keys/wg0_prv";
        };
        wireguardPeers = [
          {
            wireguardPeerConfig = {
              PublicKey = "Lp8yZR1MeCTRn/OS6uOr7R1UagYu9G+grUTLR3i3QHA=";
              PresharedKeyFile = "/var/wireguard-keys/wg0_psk";
              AllowedIPs = [ "172.21.21.0/24" "fd88:3f9f:1aa1:babe::/64" ];
              Endpoint = "146.59.3.24:63636";
              PersistentKeepalive = 25;
            };
          }
        ];
      };
    };
    networks = {
      "ethernet" = {
        matchConfig.Name = "enp*s*";
        networkConfig = {
          DHCP = "yes";
        };
      };
      "wg0" = {
        matchConfig.Name = "wg0";
        DHCP = "no";
        address = [ "172.21.21.6/32" "fd88:3f9f:1aa1:babe::6/64" ];
        routes = [
          { routeConfig = { Gateway = "172.21.21.6"; Destination = "172.21.21.0/24"; }; }
        ];
      };
    };
  };
  systemd.services."systemd-networkd".environment.SYSTEMD_LOG_LEVEL = "debug";


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
    nvidiaSettings = true;

    # Optionally, you may need to select the appropriate driver version for your specific GPU.
    package = config.boot.kernelPackages.nvidiaPackages.stable;
  };


  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "23.05"; # Did you read the comment?

}
