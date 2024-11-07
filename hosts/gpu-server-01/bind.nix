{
  config,
  lib,
  pkgs,
  ...
}:
let
  root-hints = pkgs.fetchurl {
    url = "http://www.internic.net/domain/named.root";
    hash = "sha256-I8RXSid7HOE6q7IVcA6qQTOO/7QZQ2/mh7oHVLA/U6g=";
  };
in
{
  # DNS
  services.bind = {
    enable = true;
    listenOn = [ "any" ];
    listenOnIpv6 = [ "any" ];
    cacheNetworks = [ "any" ];
    forwarders = lib.mkForce [ ];
    extraOptions = ''
      recursion yes;
      dnssec-validation auto;
      listen-on tls ephemeral {
        any;
      };

      listen-on-v6 tls ephemeral {
        any;
      };
      allow-query-cache { any; };

      max-cache-size 1G;
    '';
    extraConfig = ''
      logging {
        category security { null; };
      };

      zone "." IN {
        type hint;
        file "${root-hints}";
      };
    '';
    zones = {
      "zelcon" = {
        master = true;
        file = "/run/secrets/dns-zones/zelcon";
      };
    };
  };
  networking.nameservers = [
    "127.0.0.1"
    "::1"
  ];
}
