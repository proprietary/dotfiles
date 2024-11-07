{ config, pkgs, ... }:

{
  imports = [
    ./hardware-configuration.nix
    ./sops.nix
    ./zelcon.net-vpn.nix
    ./intel-mac-power-on.nix
    ./apple-compat.nix
  ];

  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "macmini"; # Define your hostname.
  networking.hostId = "193c4c8e";

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Set your time zone.
  time.timeZone = "America/Los_Angeles";

  # Select internationalisation properties.
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

  programs.zsh.enable = true;
  users.defaultUserShell = pkgs.zsh;

  security.sudo.wheelNeedsPassword = false;

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  nix.settings.experimental-features = [
    "nix-command"
    "flakes"
  ];

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    coreutils
    home-manager
    tmux
    emacs
    wget
    curl
    gitFull
    git-crypt
    gcc
    gnumake
    libgccjit
    pkg-config
    autoconf
    gnum4
    gdb
    llvm_18
    ruby_3_3
    sbcl
    python313
    go
    lua
    rustup
    gnused
    gnutar
    gawk
    zip
    xz
    unzip
    p7zip
    zstd
    wakelan
    ethtool
    nmap
    sysstat
    lm_sensors
    pciutils # lspci
    usbutils # lsusb
    mtr # A network diagnostic tool
    iperf3
    dnsutils # `dig` + `nslookup`
    ldns # replacement of `dig`, it provide the command `drill`
    aria2 # A lightweight multi-protocol & multi-source command-line download utility
    socat # replacement of openbsd-netcat
    nmap # A utility for network discovery and security auditing
    ipcalc # it is a calculator for the IPv4/v6 addresses
    file
    which
    strace
    lsof
    ltrace
    dmidecode
    ffmpeg-full
    nfs-utils
    btrfs-progs
    docker
    k3s
    nodejs_22
    tree-sitter
    gnupg
    age
    sops
    wireguard-tools
  ];
  environment.variables.editor = "emacs";

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
    settings = {
      TrustedUserCAKeys = "/run/secrets/net_zelcon/ssh_CA_pub";
    };
  };

  programs.mosh = {
    enable = true;
    openFirewall = true;
  };

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  networking.firewall = {
    enable = true;
  };

  networking.interfaces."enp*s*".wakeOnLan = {
    enable = true;
    policy = [
      "magic"
      "broadcast"
    ];
  };

  # Enable networking
  networking = {
    networkmanager.enable = false;
    useNetworkd = true;
  };
  systemd.network = {
    enable = true;
    networks."ethernet" = {
      matchConfig.Name = "enp*s*";
      networkConfig = {
        DHCP = "yes";
        IPMasquerade = "both";
        IPv6PrivacyExtensions = "yes";
      };
    };
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "24.05"; # Did you read the comment?

}
