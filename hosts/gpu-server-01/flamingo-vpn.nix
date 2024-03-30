{ config, ... }:
{
  systemd.network = {
    netdevs = {
      "91-wg1" = {
        netdevConfig = {
          Kind = "wireguard";
          Name = "wg1";
        };
        wireguardConfig = {
          PrivateKeyFile = config.sops.secrets."flamingo/wg/prv".path;
          ListenPort = 63636;
        };
        wireguardPeers = [
          {
            wireguardPeerConfig = {
              PublicKey = builtins.readFile ../../secrets-eval-time-secrets/flamingo_omen_pubkey;
              PresharedKeyFile = config.sops.secrets."flamingo/wg/omen/psk".path;
              AllowedIPs = ["172.21.22.1/32" "fd02:9068:ef84:babe::/128"];
              PersistentKeepalive = 25;
            };
            wireguardPeerConfig = {
              PublicKey = builtins.readFile ../../secrets/eval-time-secrets/flamingo_mbp_pubkey;
              PresharedKeyFile = config.sops.secrets."flamingo/wg/mbp/psk".path;
              AllowedIPs = ["172.21.22.1/32" "fd02:9068:ef84:babe::2/128"];
              PersistentKeepalive = 25;
            };
          }
        ];
      };
    };
    networks."91-wg0" = {
      matchConfig.Name = "wg1";
      networkConfig = {
        Address = [ "172.21.22.0/24" "fd02:9068:ef84:babe::0/64" ];
      };
    };
  };
}
