{ config, pkgs, ... }:
{
  systemd.services.enable-auto-power-on = {
    enable = true;
    description = "Enable auto power-on after power loss on Intel Mac Mini";
    wantedBy = [ "sysinit.target" ];
    serviceConfig = {
      Type = "oneshot";
      #
      # See: https://www.mythic-beasts.com/support/servers/colo/macmini
      #
      # For others it's different:
      # ... for an NVidia Mac Mini
      # setpci -s 00:03.0 0x7b.b=0x19
      # ... for a Unibody Mac Mini
      # setpci -s 0:3.0 -0x7b=20
      #
      # To find out for sure:
      #
      #     # lspci | grep LCP
      #     00:1f.0 ISA bridge: Intel Corporation Cannon Lake LPC Controller (rev 10)
      #     ^^^^^^^ Use that in the setpci command
      #
      ExecStart = ''
        ${pkgs.pciutils}/bin/setpci -s 0:1f.0 0xa4.b=0
      '';
      User = "root";
      Restart = "on-failure";
      RestartSec = "0";
    };
  };
}
