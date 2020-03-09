args@{ system ? builtins.currentSystem
, iosSdkVersion ? "10.2"
, obelisk ? (import ./.obelisk/impl { inherit system iosSdkVersion; })
, pkgs ? obelisk.reflex-platform.nixpkgs
, withHoogle ? false
}:
with obelisk;
let
  # All the versions that the user cares about are here so that they can
  # be changed in one place
  chainweaverVersion = "1.2";
  appName = "Kadena Chainweaver";
  macReleaseNumber = "0";
  linuxReleaseNumber = "0";
  ovaReleaseNumber = "0";

  obApp = import ./obApp.nix args;
  pactServerModule = import ./pact-server/service.nix;
  sass = pkgs.runCommand "sass" {} ''
    set -eux
    mkdir $out
    ${pkgs.sass}/bin/sass ${./backend/sass}/index.scss $out/sass.css
  '';
  macApp = (import ./mac.nix) {
    inherit obApp pkgs appName sass chainweaverVersion macReleaseNumber;
  };
  homeManagerModule = obelisk.reflex-platform.hackGet ./dep/home-manager + /nixos;
  linuxApp = (import ./linux.nix) {
    inherit obApp pkgs appName sass homeManagerModule chainweaverVersion linuxReleaseNumber ovaReleaseNumber;
  };
in obApp // rec {
  inherit sass;
  inherit (macApp) mac deployMac;
  inherit (linuxApp) nixosExe deb chainweaverVM chainweaverVMSystem;

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

  ci =
    let cross = {
          inherit (obApp) exe;
          inherit (obApp.ghc) desktop;
          shell = obApp.shells.ghc;
        };
    in {
      mac   = cross // { inherit mac; };
      linux = cross // { inherit (linuxApp) nixosExe deb chainweaverVM chainweaverVMSystem; };
    };
}
