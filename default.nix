{ system ? builtins.currentSystem
, iosSdkVersion ? "10.2"
, obelisk ? (import ./.obelisk/impl { inherit system iosSdkVersion; })
, pkgs ? obelisk.reflex-platform.nixpkgs
}:
with obelisk;
let
  obApp = import ./obApp.nix { inherit system iosSdkVersion obelisk pkgs; };
  pactServerModule = import ./pact-server/service.nix;

in obApp // {
  server = args@{ hostName, adminEmail, routeHost, enableHttps, version }:
    let
      exe = serverExe
        obApp.ghc.backend
        obApp.ghcjs.frontend
        obApp.passthru.staticFiles
        obApp.passthru.__closureCompilerOptimizationLevel
        version;

      nixos = import (pkgs.path + /nixos);
      # Check whether everything we need is in place.
      checkDeployment = v:
        let
          checked = runCommand "backend-config-check" {} ''
            ${obApp.ghc.backend}/backend check-deployment
            mkdir $out
            touch $out/passed-check
          '';
        in
          if lib.pathExists (checked.out + /passed-check) # Dummy check to force evaluation.
          then v
          else abort ("Ok, we should actually never get here ... that's weird!");
    in nixos {
      system = "x86_64-linux";
      configuration = checkDeployment {
        imports = [
          (obelisk.serverModules.mkBaseEc2 args)
          (obelisk.serverModules.mkObeliskApp (args//{inherit exe;}))
          # (pactServerModule {
          #   hostName = routeHost;
          #   inherit obApp pkgs;
            # The exposed port of the pact backend (proxied by nginx).
          #   nginxPort = 7011;
          #   pactPort = 7010;
          #   pactDataDir = "/var/lib/pact-web";
          #   pactUser = "pact";
          # })
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

