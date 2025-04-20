{ pkgs, ... }:
{
  boot.kernelModules = [
    "kvm-intel"
    "vhost_net"
  ];
  virtualisation.libvirtd.enable = true;
  environment.systemPackages = with pkgs; [
    virt-manager
    virt-viewer
    qemu
  ];
  users.users.zds.extraGroups = [ "libvirtd" ];
  # networking.bridges.br0.interfaces = [ "eno1" ]; # TODO(zds): This bricked my network; try again later.
  # networking.interfaces.br0.useDHCP = true;
}
