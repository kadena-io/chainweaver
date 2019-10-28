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
          pactDataDir = "/var/lib/chainweaver";
          pactUser = "pact";
          inherit hostName obApp pkgs;
        })
      ];
      services.nginx.enable = true;
    };
  }

