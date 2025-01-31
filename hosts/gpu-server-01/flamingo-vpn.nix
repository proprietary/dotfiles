{ config, ... }:
{
  systemd.network = {
    netdevs = {
      "91-wg1" = {
        netdevConfig = {
          Kind = "wireguard";
          Name = "wg1";
          MTUBytes = "1420";
        };
        wireguardConfig = {
          PrivateKeyFile = config.sops.secrets."flamingo/wg/prv".path;
          ListenPort = 63636;
        };
        wireguardPeers = [
          {
            PublicKey = builtins.readFile ../../secrets/eval-time-secrets/flamingo/wg/omen/pubkey;
            PresharedKeyFile = config.sops.secrets."flamingo/wg/omen/psk".path;
            AllowedIPs = [
              "172.21.22.1/32"
              "fd02:9068:ef84:babe::1/128"
            ];
            PersistentKeepalive = 25;
          }
          {
            PublicKey = builtins.readFile ../../secrets/eval-time-secrets/flamingo/wg/iphonex/pubkey;
            PresharedKeyFile = config.sops.secrets."flamingo/wg/iphonex/psk".path;
            AllowedIPs = [
              "172.21.22.4/32"
              "fd02:9068:ef84:babe::4/128"
            ];
            PersistentKeepalive = 25;
          }
          {
            PublicKey = builtins.readFile ../../secrets/eval-time-secrets/flamingo/wg/mbp/pubkey;
            PresharedKeyFile = config.sops.secrets."flamingo/wg/mbp/psk".path;
            AllowedIPs = [
              "172.21.22.2/32"
              "fd02:9068:ef84:babe::2/128"
            ];
          }
          {
            PublicKey = builtins.readFile ../../secrets/eval-time-secrets/flamingo/wg/iphone15/pubkey;
            PresharedKeyFile = config.sops.secrets."flamingo/wg/iphone15/psk".path;
            AllowedIPs = [
              "172.21.22.3/32"
              "fd02:9068:ef84:babe::3/128"
            ];
            PersistentKeepalive = 25;
          }
          {
            PublicKey = builtins.readFile ../../secrets/eval-time-secrets/flamingo/wg/raspi/pubkey;
            PresharedKeyFile = config.sops.secrets."flamingo/wg/raspi/psk".path;
            AllowedIPs = [ "172.21.22.5/32" ];
            PersistentKeepalive = 25;
          }
        ];
      };
    };
    networks."91-wg1" = {
      matchConfig.Name = "wg1";
      networkConfig = {
        Address = [ "172.21.22.0/24" ];
        IPMasquerade = "both";
      };

    };
  };
}
