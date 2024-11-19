{ config, lib, ... }:
let
  wgListenPort = 63637;
in
{
  #systemd.services."systemd-networkd".environment.SYSTEMD_LOG_LEVEL = "debug";
  networking.firewall.allowedUDPPorts = [ wgListenPort ];
  networking.firewall.trustedInterfaces = [ "zelcon" ];
  systemd.network = {
    netdevs."90-zelcon" = {
      netdevConfig = {
        Kind = "wireguard";
        Name = "zelcon";
      };
      wireguardConfig = {
        PrivateKeyFile = config.sops.secrets."net_zelcon/superstorage/prv".path;
        ListenPort = wgListenPort;
      };
      wireguardPeers = [
        {
          Endpoint = builtins.readFile ./../../secrets/eval-time-secrets/net_zelcon/poland/endpoint;
          PublicKey = builtins.readFile ./../../secrets/eval-time-secrets/net_zelcon/poland/pubkey;
          PresharedKeyFile = config.sops.secrets."net_zelcon/superstorage/psk".path;
          AllowedIPs = [
            "172.21.21.0/24"
            "fd88:3f9f:1aa1:babe::/64"
          ];
          PersistentKeepalive = 1;
        }
        {
          Endpoint = builtins.readFile ./../../secrets/eval-time-secrets/net_zelcon/gpu-server-01/endpoint;
          PublicKey = builtins.readFile ./../../secrets/eval-time-secrets/net_zelcon/gpu-server-01/pubkey;
          PresharedKeyFile = config.sops.secrets."net_zelcon/gpu-server-01/psk".path;
          AllowedIPs = [
            "172.21.21.6/32"
            "fd88:3f9f:1aa1:babe::6/128"
          ];
          PersistentKeepalive = 1;
        }
        {
          PublicKey = builtins.readFile ./../../secrets/eval-time-secrets/net_zelcon/mbp2023/pubkey;
          PresharedKeyFile = config.sops.secrets."net_zelcon/superstorage/psk".path;
          AllowedIPs = [
            "172.21.21.5/32"
            "fd88:3f9f:1aa1:babe::5/128"
          ];
          PersistentKeepalive = 5;
        }
        {
          PublicKey = builtins.readFile ./../../secrets/eval-time-secrets/net_zelcon/iphone15/pubkey;
          PresharedKeyFile = config.sops.secrets."net_zelcon/superstorage/psk".path;
          AllowedIPs = [
            "172.21.21.7/32"
            "fd88:3f9f:1aa1:babe::7/128"
          ];
          PersistentKeepalive = 25;
        }
        {
          Endpoint = builtins.readFile ../../secrets/eval-time-secrets/net_zelcon/raspi/endpoint;
          PublicKey = builtins.readFile ../../secrets/eval-time-secrets/net_zelcon/raspi/pubkey;
          PresharedKeyFile = config.sops.secrets."net_zelcon/raspi/psk".path;
          AllowedIPs = [ "172.21.21.4/32" ];
          PersistentKeepalive = 1;
        }
        {
          PublicKey = builtins.readFile ../../secrets/eval-time-secrets/net_zelcon/macmini/pubkey;
          PresharedKeyFile = config.sops.secrets."net_zelcon/macmini/psk".path;
          AllowedIPs = [
            "172.21.21.8/32"
            "fd88:3f9f:1aa1:babe::8/128"
          ];
          PersistentKeepalive = 1;
        }
      ];
    };
    networks."90-zelcon" = {
      matchConfig.Name = "zelcon";
      networkConfig = {
        Address = [
          "172.21.21.1/32"
          "fd88:3f9f:1aa1:babe::1/128"
        ];
      };
      routes = [
        {
          Gateway = "172.21.21.0";
          Source = "172.21.21.0/24";
          Destination = "172.21.21.0/24";
          GatewayOnLink = true;
        }
      ];
    };
  };
}
