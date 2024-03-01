{ config, ... }:
let
  net_zelcon_wg_endpoint = ../../eval-time-secrets/net_zelcon_wg_endpoint;
  endpoint = builtins.readFile net_zelcon_wg_endpoint;
  net_zelcon_wg_pubkey = ../../eval-time-secrets/net_zelcon_wg_pubkey;
  pubkey = builtins.readFile net_zelcon_wg_pubkey;
in
{
  systemd.network = {
    netdevs = {
      "90-wg0" = {
        netdevConfig = {
          Kind = "wireguard";
          Name = "wg0";
        };
        wireguardConfig = {
          PrivateKeyFile = config.sops.secrets."net_zelcon/wg/prv".path;
        };
        wireguardPeers = [
          {
            wireguardPeerConfig = {
              PublicKey = pubkey;
              Endpoint = endpoint;
              PresharedKeyFile = config.sops.secrets."net_zelcon/wg/psk".path;
              AllowedIPs = ["172.21.21.0/24" "fd88:3f9f:1aa1:babe::/64"];
              PersistentKeepalive = 25;
            };
          }
        ];
      };
    };
    networks."90-wg0" = {
      matchConfig.Name = "wg0";
      networkConfig = {
        Address = [ "172.21.21.6/32" "fd88:3f9f:1aa1:babe::6/128" ];
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
