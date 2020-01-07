args@{ system ? builtins.currentSystem
, iosSdkVersion ? "10.2"
, obelisk ? (import ./.obelisk/impl { inherit system iosSdkVersion; })
, pkgs ? obelisk.reflex-platform.nixpkgs
, withHoogle ? false
}:
with obelisk;
let
  obApp = import ./obApp.nix args;
  pactServerModule = import ./pact-server/service.nix;
  sass = pkgs.runCommand "sass" {} ''
    set -eux
    mkdir $out
    ${pkgs.sass}/bin/sass ${./backend/sass}/index.scss $out/sass.css
  '';
  appName = "Kadena Chainweaver Beta";
  macApp = (import ./mac.nix) {
    inherit obApp pkgs appName sass;
  };
  linuxApp = (import ./linux.nix) {
    inherit obApp pkgs appName sass;
  };
in obApp // rec {
  inherit sass;
  inherit (macApp) mac deployMac;
  inherit (linuxApp) nixosExe deb;

  server = args@{ hostName, adminEmail, routeHost, enableHttps, version }:
    let
      exe = serverExe
        obApp.ghc.backend
        obApp.ghcjs.frontend
        obApp.passthru.staticFiles
        obApp.passthru.__closureCompilerOptimizationLevel
        version;

      nixos = import (pkgs.path + /nixos);
    in nixos {
      system = "x86_64-linux";
      configuration = {
        imports = [
          (obelisk.serverModules.mkBaseEc2 args)
          (obelisk.serverModules.mkObeliskApp (args//{inherit exe;}))
          # Make sure all configs present:
          # (pactServerModule {
          #   hostName = routeHost;
          #   inherit obApp pkgs;
            # The exposed port of the pact backend (proxied by nginx).
          #   nginxPort = 7011;
          #   pactPort = 7010;
          #   pactDataDir = "/var/lib/chainweaver";
          #   pactUser = "pact";
          # })
        ];
        system.activationScripts = {
          setupBackendRuntime = {
            text = ''
                mkdir -p /var/lib/chainweaver/dyn-configs
              '';
            deps = [];
          };
        };
      };
    };

  ci = {
    mac   = { inherit mac; };
    linux = { };
    cross = {
      inherit (obApp) exe;
      inherit (obApp.ghc) desktop;
      shell = obApp.shells.ghc;
    };
  };
}
