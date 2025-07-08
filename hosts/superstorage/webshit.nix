{ config, ... }:
{
  security.acme = {
    acceptTerms = true;
    defaults.email = "zelcon@zelcon.net";
  };

  security.acme.certs."superstorage.internal.zelcon.net" = {
    extraDomainNames = [ "*.superstorage.internal.zelcon.net" ];
    dnsProvider = "cloudflare";
    environmentFile = config.sops.secrets."net_zelcon/cloudflare".path;
  };

  networking.firewall.allowedTCPPorts = [ 80 443 ];
}
