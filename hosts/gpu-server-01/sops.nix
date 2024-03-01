{ config, ... }:
{
  # Secrets management
  sops = {
    defaultSopsFile = ../../secrets/secrets.yaml;
    age = {
      sshKeyPaths = [
        "/etc/ssh/ssh_host_ed25519_key"
      ];
    };
    secrets."net_zelcon/wg/psk" = {
      # See: systemd-netdev(5)
      group = config.users.users.systemd-network.name;
      mode = "0640";
    };
    secrets."net_zelcon/wg/prv" = {
      # See: systemd-netdev(5)
      group = config.users.users.systemd-network.name;
      mode = "0640";
    };
    secrets."net_zelcon/ssh_CA_pub" = {
      mode = "0640";
    };
  };
}
