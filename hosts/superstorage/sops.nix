{ config, ... }:
let
  networkdSecretPaths = [
    "net_zelcon/macmini/prv"
    "net_zelcon/macmini/psk"
    "net_zelcon/superstorage/psk"
    "net_zelcon/superstorage/prv"
    "net_zelcon/mbp2023/psk"
    "net_zelcon/iphone15/psk"
    "net_zelcon/raspi/psk"
    "net_zelcon/gpu-server-01/prv"
    "net_zelcon/gpu-server-01/psk"
    "net_zelcon/macmini/psk"
  ];
  makeNetworkdSecret = path: {
    "${path}" = {
      # See: systemd-netdev(5)
      group = config.users.users.systemd-network.name;
      mode = "0640";
    };
  };
  networkdSecrets = builtins.foldl' (
    acc: path: acc // (makeNetworkdSecret path)
  ) { } networkdSecretPaths;
in
{
  sops = {
    defaultSopsFile = ../../secrets/secrets.yaml;
    age = {
      sshKeyPaths = [ "/etc/ssh/ssh_host_ed25519_key" ];
    };
    secrets = {
      "net_zelcon/ssh_CA_pub" = {
        mode = "0640";
      };
    } // networkdSecrets;
  };
}
