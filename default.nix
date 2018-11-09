{ system ? builtins.currentSystem # TODO: Get rid of this system cruft
, iosSdkVersion ? "10.2"
, obelisk ? (import ./.obelisk/impl { inherit system iosSdkVersion; })
, pkgs ? obelisk.reflex-platform.nixpkgs
}:
with obelisk;
let
  obApp = import ./obApp.nix { inherit system iosSdkVersion obelisk pkgs; };
  pactServerModule = import ./pact-server/service.nix;

in obApp // {
  server = args@{ hostName, adminEmail, routeHost, enableHttps, config, version }:
    let
      nixos = import (pkgs.path + /nixos);
      # Check whether everything we need is in place.
      checkDeployment = v:
        let
          hasServerList = lib.pathExists (config + "/common/pact-server-list");
        in
          if hasServerList
          then v
          else abort ("\n\n========================= PACT-WEB ERROR =========================\n\n" +
                          "For deployments you have to provide a common/pact-server-list file\n" +
                          "in your deployment config directory, check README.md for details!\n\n" +
                          "==================================================================\n\n");

    in nixos {
      system = "x86_64-linux";
      configuration = checkDeployment {
        imports = [
          (obelisk.serverModules.mkBaseEc2 args)
          (obelisk.serverModules.mkObeliskApp (args // { exe = obApp.linuxExeConfigurable (pkgs.copyPathToStore config) version; }))
          (pactServerModule {
            hostName = routeHost;
            inherit obApp pkgs;
            # The exposed port of the pact backend (proxied by nginx).
            nginxPort = 7011;
            pactPort = 7010;
            pactDataDir = "/var/lib/pact-web";
            pactUser = "pact";
          })
        ];
        system.activationScripts = {
          setupBackendRuntime = {
            text = ''
                mkdir -p /var/lib/pact-web/dyn-configs
              '';
            deps = [];
          };
        };
      };
    };
}

