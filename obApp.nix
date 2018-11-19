{ system ? builtins.currentSystem # TODO: Get rid of this system cruft
, iosSdkVersion ? "10.2"
, obelisk ? (import ./.obelisk/impl { inherit system iosSdkVersion; })
, pkgs ? obelisk.reflex-platform.nixpkgs
}:
with obelisk;
let
  optionalExtension = cond: overlay: if cond then overlay else _: _: {};
  getGhcVersion = ghc: if ghc.isGhcjs or false then ghc.ghcVersion else ghc.version;
  haskellLib = pkgs.haskell.lib;
in
  project ./. ({ pkgs, ... }: {
    # android.applicationId = "systems.obsidian.obelisk.examples.minimal";
    # android.displayName = "Obelisk Minimal Example";
    # ios.bundleIdentifier = "systems.obsidian.obelisk.examples.minimal";
    # ios.bundleName = "Obelisk Minimal Example";

    __closureCompilerOptimizationLevel = "SIMPLE";

    shellToolOverrides = ghc: super: {
         z3 = pkgs.z3;
       };

    overrides = let
      inherit (pkgs) lib;
      guard-ghcjs-overlay = self: super:
        let hsNames = [ "cacophony" "haskeline" "katip" "ridley" ];
        in lib.genAttrs hsNames (name: null);
      ghcjs-overlay = self: super: {
        # tests rely on doctest
        bytes = haskellLib.dontCheck super.bytes;
        # tests rely on doctest
        lens-aeson = haskellLib.dontCheck super.lens-aeson;
        # tests rely on doctest
        trifecta = haskellLib.dontCheck super.trifecta;
        # tests rely on doctest
        bound = haskellLib.dontCheck super.bound;
        # extra-tests is failing
        extra = haskellLib.dontCheck super.extra;
        # tests hang
        algebraic-graphs = haskellLib.dontCheck super.algebraic-graphs;
        # hw-hspec-hedgehog doesn't work
        pact = haskellLib.dontCheck super.pact;
      };
      common-overlay = self: super: {

            intervals = pkgs.haskell.lib.dontCheck super.intervals;

            pact = pkgs.haskell.lib.overrideCabal (self.callCabal2nix "pact" (pkgs.fetchFromGitHub {
              owner = "kadena-io";
              repo = "pact";
              rev = "167e0ae5bff236aff21e5d98c32bd53a8d114cb7";
              sha256 = "00hw5yrjg642zbkpn88w4j8z8kqcsxsf39p60v6szq6xlaw0k1bg";
            }) {}) (drv: {
              testSystemDepends = (drv.testSystemDepends or []) ++ [ pkgs.z3 ];
              doCheck = false;
              executableToolDepends = (drv.executableToolDepends or []) ++ [ pkgs.makeWrapper ];
              postInstall = ''
                wrapProgram $out/bin/pact --prefix PATH : "${pkgs.z3}/bin/"
              '';
            });

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

            reflex-dom-contrib = (self.callCabal2nix "reflex-dom-contrib" (pkgs.fetchFromGitHub {
              owner = "reflex-frp";
              repo = "reflex-dom-contrib";
              rev = "9900f2d433240a3f93cdae930a6ffbb73c50bb86";
              sha256 = "1z8cnnhibsiap08pq2iw1r5zqvbla6hci7dhrz9mhfr0nqyryk65";
            }) {});

            # ghc-8.0.2 haddock has an annoying bug, which causes build failures:
            # See: https://github.com/haskell/haddock/issues/565
            frontend = pkgs.haskell.lib.dontHaddock super.frontend;
          };
    in self: super: lib.foldr lib.composeExtensions (_: _: {}) [
      common-overlay
      (optionalExtension (super.ghc.isGhcjs or false) guard-ghcjs-overlay)
      (optionalExtension (super.ghc.isGhcjs or false) ghcjs-overlay)
    ] self super;
  })
