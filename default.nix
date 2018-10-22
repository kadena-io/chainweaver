{ system ? builtins.currentSystem # TODO: Get rid of this system cruft
, iosSdkVersion ? "10.2"
, obelisk ? (import ./.obelisk/impl { inherit system iosSdkVersion; __useLegacyCompilers = ghc80; })
, pkgs ? obelisk.reflex-platform.nixpkgs
, ghc80 ? true
}:
with obelisk;
let
  optionalExtension = cond: overlay: if cond then overlay else _: _: {};
  getGhcVersion = ghc: if ghc.isGhcjs or false then ghc.ghcVersion else ghc.version;
  haskellLib = pkgs.haskell.lib;
  obApp = project ./. ({ pkgs, ... }: {
    # android.applicationId = "systems.obsidian.obelisk.examples.minimal";
    # android.displayName = "Obelisk Minimal Example";
    # ios.bundleIdentifier = "systems.obsidian.obelisk.examples.minimal";
    # ios.bundleName = "Obelisk Minimal Example";

    overrides = let
      inherit (pkgs) lib;
      semantic-reflex-src = pkgs.fetchFromGitHub {
        owner = "tomsmalley";
        repo = "semantic-reflex";
        rev = "42bfede5e308bab4494e87ed0144f21134a4c5b3";
        sha256 = "01rpf0vh5llx1hq4j55gmw36fvzhb95ngcykh34sgcxp5498p9f3";
      };
      guard-ghcjs-overlay = self: super:
        let hsNames = [ "cacophony" "haskeline" "katip" "ridley" ];
        in lib.genAttrs hsNames (name: null);
      common-overlay = self: super: {
            # algebraic-graphs = pkgs.haskell.lib.dontCheck super.algebraic-graphs;
            # cacophony = pkgs.haskell.lib.dontCheck (self.callHackage "cacophony" "0.8.0" {});
            # haskeline = self.callHackage "haskeline" "0.7.4.2" {};
            # katip = pkgs.haskell.lib.doJailbreak (self.callHackage "katip" "0.3.1.4" {});
            # ridley = pkgs.haskell.lib.dontCheck (self.callHackage "ridley" "0.3.1.2" {});

            # bound = pkgs.haskell.lib.dontCheck super.bound;
            # bytes = pkgs.haskell.lib.dontCheck super.bytes;
            # doctest = self.callHackage "doctest" "0.16.0" {};
            # extra = pkgs.haskell.lib.dontCheck super.extra;
            # io-streams = pkgs.haskell.lib.dontCheck super.io-streams;
            # lens-aeson = pkgs.haskell.lib.dontCheck super.lens-aeson;
            # trifecta = pkgs.haskell.lib.dontCheck super.trifecta;

            pact = pkgs.haskell.lib.dontCheck (self.callCabal2nix "pact" (pkgs.fetchFromGitHub {
              owner = "kadena-io";
              repo = "pact";
              rev = "3fd9d9d470069281b0b96e802cb990a210392284";
              sha256 = "1lihkkhx2gkld9gkyz1dfnbv1hq44bzswjv9g9fkhrjmf8xv4wav";
            }) {});

            reflex-dom-ace = (self.callCabal2nix "reflex-dom-ace" (pkgs.fetchFromGitHub {
              owner = "reflex-frp";
              repo = "reflex-dom-ace";
              rev = "24e1ee4b84f50bd5b6b4401c4bdc28963ce8d80f";
              sha256 = "0hdn00cd17a7zp56krqs3y5mpcml75pn8mnmhwyixqgscqd1q9y5";
            }) {});

            # sbv >= 7.9
            sbv = pkgs.haskell.lib.dontCheck (self.callCabal2nix "sbv" (pkgs.fetchFromGitHub {
              owner = "LeventErkok";
              repo = "sbv";
              rev = "3dc60340634c82f39f6c5dca2b3859d10925cfdf";
              sha256 = "18xcxg1h19zx6gdzk3dfs87447k3xjqn40raghjz53bg5k8cdc31";
            }) {});

            thyme = pkgs.haskell.lib.dontCheck (pkgs.haskell.lib.enableCabalFlag (self.callCabal2nix "thyme" (pkgs.fetchFromGitHub {
              owner = "kadena-io";
              repo = "thyme";
              rev = "6ee9fcb026ebdb49b810802a981d166680d867c9";
              sha256 = "09fcf896bs6i71qhj5w6qbwllkv3gywnn5wfsdrcm0w1y6h8i88f";
            }) {}) "ghcjs");

            semantic-reflex = pkgs.haskell.lib.dontHaddock (pkgs.haskell.lib.doJailbreak (pkgs.haskell.lib.dontCheck (self.callCabal2nix "semantic-reflex"  (semantic-reflex-src + /semantic-reflex) {})));

            # ghc-8.0.2 haddock has an annoying bug, which causes build failures:
            # See: https://github.com/haskell/haddock/issues/565
            frontend = pkgs.haskell.lib.dontHaddock super.frontend;
          };
      ghc80-overlay = self: super: {
        criterion = self.callHackage "criterion" "1.4.0.0" {};
        megaparsec = haskellLib.dontCheck super.megaparsec;
        base-compat-batteries = haskellLib.addBuildDepend (haskellLib.doJailbreak super.base-compat-batteries) self.bifunctors;
        modern-uri = haskellLib.dontCheck super.modern-uri;
        pact = haskellLib.overrideCabal (self.callCabal2nix "pact" (pkgs.fetchFromGitHub {
          owner = "kadena-io";
          repo = "pact";
          rev = "d4418c67e66e29e2627a7cd5a4fbf32db4af5cc1";
          sha256 = "062cy72sd83wymmzxwd8gbrwh6hi9ks7m8hjbhywsrblm83gi2f3";
        }) {}) (drv: {
          # TODO relax bounds upstream
          jailbreak = true;
          # TODO Fix test for hspec version
          doCheck = false;
          # TODO upstream to pact. Fix pact build for yaml >= 8.3.1
          postPatch = (drv.postPatch or "") + ''
            substituteInPlace src/Pact/Types/Logger.hs --replace \
              "either error id $ Y.decodeEither" \
              "either (error . show) id $ Y.decodeEither'"
          '';
        });
        # statistics = haskellLib.dontCheck super.statistics;
      };
    in self: super: lib.foldr lib.composeExtensions (_: _: {}) [
      common-overlay
      (optionalExtension (lib.versionOlder (getGhcVersion super.ghc) "8.4") ghc80-overlay)
      (optionalExtension (super.ghc.isGhcjs or false) guard-ghcjs-overlay)
    ] self super;
  });
  pactServerModule = {
    certificatePath,
    certificateKeyPath,
    hostname,
    nginxPort,
    pactPort,
    pactDataDir,
    pactUser
  }: let
    pactConfig = pkgs.writeText "pact.yaml" ''
      port: ${toString pactPort}

      logDir: ${pactDataDir}
      persistDir: ${pactDataDir}

      # SQLite pragmas for pact back-end
      pragmas: []

      # verbose: provide log output
      verbose: True
    '';
  in {pkgs, lib, ...}: {
    users.users.${pactUser} = {
      description = "User for running the pact server (pact -s).";
      isSystemUser = true;
    };
    systemd.services.pact-server = {
      description = "Pact Server";
      after = [ "network.target" ];
      wantedBy = [ "multi-user.target" ];
      # So preStart runs as root:
      preStart = ''
        export PATH=$PATH:${pkgs.coreutils}/bin
        mkdir -p ${pactDataDir}
        chown ${pactUser} ${pactDataDir}
        '';
      serviceConfig = {
        # So preStart runs as root:
        PermissionsStartOnly = true;
        User = pactUser;
        ExecStart = "${obApp.ghc.pact}/bin/pact -s ${pactConfig}";
        Restart = "always";
        KillMode = "process";
      };
    };
    networking.firewall.allowedTCPPorts = [ 22 80 443 nginxPort ];
    services.nginx.appendHttpConfig = ''
        server {
          listen 0.0.0.0:${toString nginxPort} ssl;
          listen [::]:${toString nginxPort} ssl;
          server_name https://${hostname};
          ssl_certificate ${certificatePath};
          ssl_certificate_key ${certificateKeyPath};

          # Restrict transaction size:
          client_max_body_size 1m;

          location / {
            if ($request_method = 'POST') {
               # add_header 'Access-Control-Allow-Origin' 'https://working-agreement.obsidian.systems';
               add_header 'Access-Control-Allow-Methods' 'POST';
               add_header 'Access-Control-Allow-Headers' 'DNT,User-Agent,X-Requested-With,If-Modified-Since,Cache-Control,Content-Type,Range';
               add_header 'Access-Control-Expose-Headers' 'Content-Length,Content-Range';
            }


            proxy_pass http://127.0.0.1:${toString pactPort};
            proxy_http_version 1.1;
            # proxy_set_header Upgrade $http_upgrade;
            # proxy_set_header Connection "upgrade";
          }
        }
    '';
  };
in obApp // {
  server = args@{ hostName, adminEmail, routeHost, enableHttps, config }:
    let
      nixos = import (pkgs.path + /nixos);
    in nixos {
      system = "x86_64-linux";
      configuration = {
        imports = [
          (obelisk.serverModules.mkBaseEc2 args)
          (obelisk.serverModules.mkObeliskApp (args // { exe = obApp.linuxExeConfigurable (pkgs.copyPathToStore config); }))
          (pactServerModule {
            certificatePath = "/var/lib/acme/working-agreement.obsidian.systems/fullchain.pem";
            certificateKeyPath = "/var/lib/acme/working-agreement.obsidian.systems/key.pem";
            hostname = "working-agreement.obsidian.systems";
            nginxPort = 7011;
            pactPort = 7010;
            pactDataDir = "/var/lib/pact-web/pact-log";
            pactUser = "pact";
          })
        ];
      };
    };
}

