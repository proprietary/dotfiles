{ config, ... }:
{
  # Secrets management
  sops = {
    defaultSopsFile = ../../secrets/secrets.yaml;
    age = {
      sshKeyPaths = [ "/etc/ssh/ssh_host_ed25519_key" ];
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
    secrets."net_zelcon/gpu-server-01/prv" = {
      group = config.users.users.systemd-network.name;
      mode = "0640";
    };
    secrets."net_zelcon/gpu-server-01/psk" = {
      group = config.users.users.systemd-network.name;
      mode = "0640";
    };
    secrets."net_zelcon/macmini/psk" = {
      group = config.users.users.systemd-network.name;
      mode = "0640";
    };
    secrets."net_zelcon/x220/psk" = {
      group = config.users.users.systemd-network.name;
      group = "0640";
    };
    secrets."dns-zones/zelcon" = {
      group = config.users.users.named.name;
      mode = "0640";
    };
  };
}
