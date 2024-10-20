{ config, lib, ... }:
let
  networkdSecretPaths = [
    "net_zelcon/macmini/prv"
    "net_zelcon/macmini/psk"
  ];
  makeNetworkdSecret = path: {
    "${path}" = {
      # See: systemd-netdev(5)
      group = config.users.users.systemd-network.name;
      mode = "0640";
    };
  };
  networkdSecrets = builtins.foldl' (acc: path: acc // (makeNetworkdSecret path)) {} networkdSecretPaths;
in
{
  sops = {
    defaultSopsFile = ./../../secrets/secrets.yaml;
    age = {
      sshKeyPaths = [
        "/etc/ssh/ssh_host_ed25519_key"
      ];
    };
    secrets = {
      "net_zelcon/ssh_CA_pub" = {
        mode = "0640";
      };
    } // networkdSecrets;
  };
}
