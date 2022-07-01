args@{ system ? builtins.currentSystem
, iosSdkVersion ? "10.2"
, terms ? { # Accepted terms, conditions, and licenses
      security.acme.acceptTerms = true;
  }
, kpkgs ? import ./dep/kpkgs { inherit system; }  # If you want a custom package set, pass it into
                                                  # kpkgs arg
, obelisk ? (import ./.obelisk/impl { inherit system iosSdkVersion terms; inherit (kpkgs) reflex-platform-func;})
, withHoogle ? false
}:
with obelisk;
let
  pkgs = obelisk.reflex-platform.nixpkgs;
  # All the versions that the user cares about are here so that they can
  # be changed in one place
  chainweaverVersion = "2.2.2";
  appName = "Kadena Chainweaver";
  macReleaseNumber = "0";
  linuxReleaseNumber = "0";
  ovaReleaseNumber = "0";

  obApp = import ./obApp.nix { inherit obelisk; };
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

  mkObeliskAppWithNginx =
    { exe
    , routeHost
    , enableHttps
    , name ? "backend"
    , user ? name
    , group ? user
    , baseUrl ? "/"
    , internalPort ? 8000
    , backendArgs ? "--port=${toString internalPort}"
    , ...
    }: {...}: {

    services.nginx = {
      enable = true;
      package = pkgs.nginx-1-22;
      recommendedProxySettings = true;
      virtualHosts."${routeHost}" = {
        enableACME = enableHttps;
        forceSSL = enableHttps;
        locations.${baseUrl} = {
          proxyPass = "http://127.0.0.1:" + toString internalPort;
          proxyWebsockets = true;
          extraConfig = ''
            access_log off;
            limit_req zone=one;
            limit_conn addr 50;
          '';
        };
      };
      appendHttpConfig = ''
        limit_req_dry_run on;
        limit_req_log_level info;
        limit_req_zone $binary_remote_addr zone=one:200m rate=30r/m;
        limit_conn_zone $binary_remote_addr zone=addr:10m;
        limit_conn_dry_run on;
        limit_conn_log_level info;
      '';
    };
    systemd.services.nginx.serviceConfig.ReadWritePaths = [ "/var/spool/nginx/logs/" ];
    systemd.services.${name} = {
      wantedBy = [ "multi-user.target" ];
      after = [ "network.target" ];
      restartIfChanged = true;
      path = [ pkgs.gnutar ];
      script = ''
        ln -sft . '${exe}'/*
        mkdir -p log
        exec ./backend ${backendArgs} </dev/null
      '';
      serviceConfig = {
        User = user;
        KillMode = "process";
        WorkingDirectory = "~";
        Restart = "always";
        RestartSec = 5;
      };
    };
    users = {
      users.${user} = {
        description = "${user} service";
        home = "/var/lib/${user}";
        createHome = true;
        isSystemUser = true;
        group = group;
      };
      groups.${group} = {};
    };
  };

in obApp // rec {
  inherit sass;
  inherit pkgs;
  inherit (macApp) mac deployMac;
  inherit (linuxApp) nixosExe deb chainweaverVM chainweaverVMSystem;

  server = { hostName, adminEmail, routeHost, enableHttps, version, module ? obelisk.serverModules.mkBaseEc2 }@args:
    let
      exe = serverExe
        obApp.ghc.backend
        obApp.ghcjs.frontend
        obApp.passthru.staticFiles
        obApp.passthru.__closureCompilerOptimizationLevel
        obApp.passthru.externjs
        version;
      nixos = import (pkgs.path + /nixos);
    in nixos {
      system = "x86_64-linux";
      configuration = {
        imports = [
          (module { inherit exe hostName adminEmail routeHost enableHttps version; nixosPkgs = pkgs; })
          (obelisk.serverModules.mkDefaultNetworking args)
          # (obelisk.serverModules.mkObeliskApp (args // { inherit exe; }))
          (mkObeliskAppWithNginx (args // { inherit exe; }))
          ./acme.nix  # Backport of ACME upgrades from 20.03
          # (pactServerModule {
          #   hostName = routeHost;
          #   inherit obApp pkgs;
          # # The exposed port of the pact backend (proxied by nginx).
          #   nginxPort = 7011;
          #   pactPort = 7010;
          #   pactDataDir = "/var/lib/chainweaver";
          #   pactUser = "pact";
          # })
        ];

        # Backport of ACME upgrades from 20.03
        disabledModules = [
          (pkgs.path + /nixos/modules/security/acme.nix)
        ];
        nixpkgs.overlays = [
          (self: super: {
            lego = (import (builtins.fetchTarball {
                url = https://github.com/NixOS/nixpkgs-channels/archive/70717a337f7ae4e486ba71a500367cad697e5f09.tar.gz;
                sha256 = "1sbmqn7yc5iilqnvy9nvhsa9bx6spfq1kndvvis9031723iyymd1";
              }) {}).lego;
          })
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
          shell = obApp.shells.ghc { withHoogle = true; };
        };
    in {
      mac   = cross // { inherit mac; };
      linux = cross // { inherit (linuxApp) nixosExe deb chainweaverVM chainweaverVMSystem; };
    };
}
