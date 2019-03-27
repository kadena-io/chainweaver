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
  project ./. ({ pkgs, hackGet, ... }: {
    # android.applicationId = "systems.obsidian.obelisk.examples.minimal";
    # android.displayName = "Obelisk Minimal Example";
    # ios.bundleIdentifier = "systems.obsidian.obelisk.examples.minimal";
    # ios.bundleName = "Obelisk Minimal Example";

    __closureCompilerOptimizationLevel = "SIMPLE";

    shellToolOverrides = ghc: super: {
         z3 = pkgs.z3;
       };
    packages = {
      pact = hackGet ./deps/pact;
      servant-client-jsaddle = (hackGet ./deps/servant) + "/servant-client-jsaddle";
      reflex-dom-ace = hackGet ./deps/reflex-dom-ace;
      reflex-dom-contrib = hackGet ./deps/reflex-dom-contrib;
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

        bsb-http-chunked = haskellLib.dontCheck super.bsb-http-chunked;
        Glob = haskellLib.dontCheck super.Glob;
        http2 = haskellLib.dontCheck super.http2;
        http-date = haskellLib.dontCheck super.http-date;
        http-media = haskellLib.dontCheck super.http-media;
        iproute = haskellLib.dontCheck super.iproute;
        markdown-unlit = haskellLib.dontCheck super.markdown-unlit;
        mockery = haskellLib.dontCheck super.mockery;
        silently = haskellLib.dontCheck super.silently;
        servant = haskellLib.dontCheck super.servant;
        servant-client = haskellLib.dontCheck super.servant-client;
        unix-time = haskellLib.dontCheck super.unix-time;
        wai-app-static = haskellLib.dontCheck super.wai-app-static;
        wai-extra = haskellLib.dontCheck super.wai-extra;
        prettyprinter-ansi-terminal = haskellLib.dontCheck (haskellLib.dontHaddock super.prettyprinter-ansi-terminal);
        prettyprinter-convert-ansi-wl-pprint = haskellLib.dontCheck (haskellLib.dontHaddock super.prettyprinter-convert-ansi-wl-pprint);

        foundation = (haskellLib.overrideCabal super.foundation (drv: {
          postPatch = (drv.postPatch or "") + pkgs.lib.optionalString (system == "x86_64-darwin") ''
            substituteInPlace foundation.cabal --replace 'if os(linux)' 'if os(linux) && !impl(ghcjs)'
            substituteInPlace foundation.cabal --replace 'if os(osx)' 'if os(linux) && impl(ghcjs)'
          '';
        }));
      };
      common-overlay = self: super: {

            intervals = pkgs.haskell.lib.dontCheck super.intervals;

            pact = pkgs.haskell.lib.overrideCabal super.pact (drv: {
              testSystemDepends = (drv.testSystemDepends or []) ++ [ pkgs.z3 ];
              doCheck = false;
              executableToolDepends = (drv.executableToolDepends or []) ++ [ pkgs.makeWrapper ];
              postInstall = ''
                wrapProgram $out/bin/pact --prefix PATH : "${pkgs.z3}/bin/"
              '';
            });


            # sbv 8.1
            sbv = pkgs.haskell.lib.dontCheck (self.callCabal2nix "sbv" (pkgs.fetchFromGitHub {
              owner = "LeventErkok";
              repo = "sbv";
              rev = "365b1a369a2550d6284608df3fbc17e2663c4d3c";
              sha256 = "134f148g28dg7b3c1rvkh85pfl9pdlvrvl6al4vlz72f3y5mb2xg";
            }) {});

            # need crackNum 2.3
            crackNum = pkgs.haskell.lib.dontCheck (self.callCabal2nix "crackNum" (pkgs.fetchFromGitHub {
              owner = "LeventErkok";
              repo = "crackNum";
              rev = "54cf70861a921062db762b3c50e933e73446c3b2";
              sha256 = "02cg64rq8xk7x53ziidljyv3gsshdpgbzy7h03r869gj02l7bxwa";
            }) {});

            # servant-client-jsaddle = haskellLib.doJailbreak (haskellLib.dontCheck (self.callCabal2nix "servant-client-jsaddle" ((pkgs.fetchFromGitHub {
            #   owner = "haskell-servant";
            #   repo = "servant";
            #   rev = "85d6471debfb4a5707c261d4b9deaa33ed4c65db";
            #   sha256 = "1lwa6kbpjmx17lkh74p9nfjiwzqcy3whza59m67k96ab3bh2b99y";
            # }) + "/servant-client-jsaddle") {}));
            servant-client-jsaddle = pkgs.haskell.lib.dontCheck (pkgs.haskell.lib.doJailbreak super.servant-client-jsaddle);

            thyme = pkgs.haskell.lib.dontCheck (pkgs.haskell.lib.enableCabalFlag (self.callCabal2nix "thyme" (pkgs.fetchFromGitHub {
              owner = "kadena-io";
              repo = "thyme";
              rev = "6ee9fcb026ebdb49b810802a981d166680d867c9";
              sha256 = "09fcf896bs6i71qhj5w6qbwllkv3gywnn5wfsdrcm0w1y6h8i88f";
            }) {}) "ghcjs");

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
