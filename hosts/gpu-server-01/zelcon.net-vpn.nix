{ config, ... }:
{
  systemd.network = {
    netdevs = {
      "90-wg0" = {
        netdevConfig = {
          Kind = "wireguard";
          Name = "wg0";
        };
        wireguardConfig = {
          PrivateKeyFile = config.sops.secrets."net_zelcon/gpu-server-01/prv".path;
          ListenPort = 63618;
        };
        wireguardPeers = [
          {
            wireguardPeerConfig = {
              PublicKey = builtins.readFile ../../secrets/eval-time-secrets/net_zelcon/poland/pubkey;
              Endpoint = builtins.readFile ../../secrets/eval-time-secrets/net_zelcon/poland/endpoint;
              PresharedKeyFile = config.sops.secrets."net_zelcon/gpu-server-01/psk".path;
              AllowedIPs = ["172.21.21.0/24" "fd88:3f9f:1aa1:babe::/64"];
              PersistentKeepalive = 1;
            };
          }
          {
            wireguardPeerConfig = {
              PublicKey = builtins.readFile ../../secrets/eval-time-secrets/net_zelcon/superstorage/pubkey;
              PresharedKeyFile = config.sops.secrets."net_zelcon/superstorage/psk".path;
              Endpoint = builtins.readFile ../../secrets/eval-time-secrets/net_zelcon/superstorage/endpoint-internal;
              AllowedIPs = ["172.21.21.1/32" "fd88:3f9f:1aa1:babe::1/128"];
              PersistentKeepalive = 25;
            };
          }
          {
            wireguardPeerConfig = {
              PublicKey = builtins.readFile ../../secrets/eval-time-secrets/net_zelcon/mbp2023/pubkey;
              PresharedKeyFile = config.sops.secrets."net_zelcon/mbp2023/psk".path;
              AllowedIPs = ["172.21.21.5/32" "fd88:3f9f:1aa1:babe::5/128"];
              PersistentKeepalive = 25;
            };
          }
          {
            wireguardPeerConfig = {
              PublicKey = builtins.readFile ../../secrets/eval-time-secrets/net_zelcon/iphone15/pubkey;
              PresharedKeyFile = config.sops.secrets."net_zelcon/iphone15/psk".path;
              AllowedIPs = ["172.21.21.7/32" "fd88:3f9f:1aa1:babe::7/128"];
              PersistentKeepalive = 25;
            };
          }
          {
            wireguardPeerConfig = {
              Endpoint = builtins.readFile ../../secrets/eval-time-secrets/net_zelcon/raspi/endpoint-internal;
              PublicKey = builtins.readFile ../../secrets/eval-time-secrets/net_zelcon/raspi/pubkey;
              PresharedKeyFile = config.sops.secrets."net_zelcon/raspi/psk".path;
              AllowedIPs = ["172.21.21.4/32" "fd88:3f9f:1aa1:babe::4/128"];
              PersistentKeepalive = 25;
            };
          }
          {
            wireguardPeerConfig = {
              PublicKey = builtins.readFile ../../secrets/eval-time-secrets/net_zelcon/macmini/pubkey;
              PresharedKeyFile = config.sops.secrets."net_zelcon/macmini/psk".path;
              AllowedIPs = [ "172.21.21.8/32" "fd88:3f9f:1aa1:babe::8/128" ];
              PersistentKeepalive = 1;
            };
          }
        ];
      };
    };
    networks."90-wg0" = {
      matchConfig.Name = "wg0";
      networkConfig = {
        Address = [ "172.21.21.6/32" "fd88:3f9f:1aa1:babe::6/128" ];
        IPMasquerade = "both";
      };
      routes = [
        {
          routeConfig = {
            Gateway = "172.21.21.0";
            Source = "172.21.21.0/24";
            Destination = "172.21.21.0/24";
            GatewayOnLink = true;
            Scope = "global";
          };
        }
      ];
    };
  };
}
