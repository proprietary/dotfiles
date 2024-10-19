{ config, ... }:
{
  sops = {
    defaultSopsFile = ./../../secrets/secrets.yaml;
    age = {
      sshKeyPaths = [
        "/etc/ssh/ssh_host_ed25519_key"
      ];
    };
    secrets."net_zelcon/ssh_CA_pub" = {
      mode = "0640";
    };
  };
}
