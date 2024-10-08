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
    secrets."flamingo/wg/prv" = {
      group = config.users.users.systemd-network.name;
      mode = "0640";
    };
    secrets."flamingo/wg/mbp/psk" = {
      group = config.users.users.systemd-network.name;
      mode = "0640";
    };
    secrets."flamingo/wg/omen/psk" = {
      group = config.users.users.systemd-network.name;
      mode = "0640";
    };
    secrets."flamingo/wg/iphone15/psk" = {
      group = config.users.users.systemd-network.name;
      mode = "0640";
    };
    secrets."flamingo/wg/iphonex/psk" = {
      group = config.users.users.systemd-network.name;
      mode = "0640";
    };
    secrets."flamingo/wg/raspi/psk" = {
      group = config.users.users.systemd-network.name;
      mode = "0640";
    };
    secrets."net_zelcon/superstorage/psk" = {
      group = config.users.users.systemd-network.name;
      mode = "0640";
    };
    secrets."net_zelcon/mbp2023/psk" = {
      group = config.users.users.systemd-network.name;
      mode = "0640";
    };
    secrets."net_zelcon/iphone15/psk" = {
      group = config.users.users.systemd-network.name;
      mode = "0640";
    };
    secrets."net_zelcon/raspi/psk" = {
      group = config.users.users.systemd-network.name;
      mode = "0640";
    };
    secrets."dns-zones/zelcon" = {
      group = config.users.users.named.name;
      mode = "0640";
    };
  };
}
