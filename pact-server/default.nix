{ system ? builtins.currentSystem # TODO: Get rid of this system cruft
, iosSdkVersion ? "10.2"
, obelisk ? (import ../.obelisk/impl { inherit system iosSdkVersion; })
, pkgs ? obelisk.reflex-platform.nixpkgs
, hostName
, adminEmail
}:
let
  obApp = import ../obApp.nix { inherit system iosSdkVersion obelisk pkgs; };
  pactServerModule = import ./service.nix;
  nixos = import (pkgs.path + /nixos);
  args = { inherit hostName adminEmail; routeHost = hostName; enableHttps = true;};
in 
  nixos {
    system = "x86_64-linux";
    configuration = {
      imports = [
        (obelisk.serverModules.mkBaseEc2 args)
        (pactServerModule {
          pactPort = 7010;
          nginxPort = 443;
          pactDataDir = "/var/lib/pact-web";
          pactUser = "pact";
          inherit hostName obApp pkgs;
        })
      ];

      security.acme.certs.${hostName} = {
        webroot = "/var/www/challenges";
        postRun = "systemctl reload nginx.service";
      };

      services.nginx.enable = true;
      services.nginx.appendHttpConfig = ''
          server {
            server_name http://${hostName};
            listen 80;
            listen [::]:80;

            location /.well-known/acme-challenge {
              root /var/www/challenges;
            }

            location / {
              return 301 https://$host$request_uri;
            }
          }
      '';
    };
  }

