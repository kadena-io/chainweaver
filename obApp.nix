{ system ? builtins.currentSystem # TODO: Get rid of this system cruft
, iosSdkVersion ? "10.2"
, obelisk ? (import ./.obelisk/impl { inherit system iosSdkVersion; })
, pkgs ? obelisk.reflex-platform.nixpkgs
, withHoogle ? false
}:
with obelisk;
let
  optionalExtension = cond: overlay: if cond then overlay else _: _: {};
  getGhcVersion = ghc: if ghc.isGhcjs or false then ghc.ghcVersion else ghc.version;
  haskellLib = pkgs.haskell.lib;
in
  project ./. ({ pkgs, hackGet, ... }: with pkgs.haskell.lib; {
    inherit withHoogle;

    # android.applicationId = "systems.obsidian.obelisk.examples.minimal";
    # android.displayName = "Obelisk Minimal Example";
    # ios.bundleIdentifier = "systems.obsidian.obelisk.examples.minimal";
    # ios.bundleName = "Obelisk Minimal Example";

    __closureCompilerOptimizationLevel = "SIMPLE";

    shellToolOverrides = ghc: super: {
         z3 = pkgs.z3;
       };
   packages =
     let
       servantSrc = hackGet ./deps/servant;
     in
       {
          pact = hackGet ./deps/pact;
          # servant-client-core = servantSrc + "/servant-client-core";
          # servant = servantSrc + "/servant";
          servant-client-jsaddle = servantSrc + "/servant-client-jsaddle";
          reflex-dom-ace = hackGet ./deps/reflex-dom-ace;
          reflex-dom-contrib = hackGet ./deps/reflex-dom-contrib;
          servant-github = hackGet ./deps/servant-github;
          obelisk-oauth-common = hackGet ./deps/obelisk-oauth + /common;
          obelisk-oauth-frontend = hackGet ./deps/obelisk-oauth + /frontend;
          obelisk-oauth-backend = hackGet ./deps/obelisk-oauth + /backend;
          # Needed for obelisk-oauth currently (ghcjs support mostly):
          entropy = hackGet ./deps/entropy;
          cardano-crypto = hackGet ./deps/cardano-crypto;
          desktop = ./desktop;
          mac = builtins.filterSource (path: type: !(builtins.elem (baseNameOf path) ["static"])) ./mac;
      };

    overrides = let
      inherit (pkgs) lib;
      desktop-overlay = self: super: {
        ether = haskellLib.doJailbreak super.ether;
      };
      guard-ghcjs-overlay = self: super:
        let hsNames = [ "cacophony" "haskeline" "katip" "ridley" ];
        in lib.genAttrs hsNames (name: null);
      ghcjs-overlay = self: super: {
        # I'm not sure if these hang or just take a long time
        hourglass = haskellLib.dontCheck super.hourglass;
        x509 = haskellLib.dontCheck super.x509;
        tls = haskellLib.dontCheck super.tls;
        x509-validation = haskellLib.dontCheck super.x509-validation;

        # doctest
        iproute = haskellLib.dontCheck super.iproute;
        swagger2 = haskellLib.dontCheck super.swagger2;
        servant-server = haskellLib.dontCheck super.servant-server;

        # failing
        wai-extra = haskellLib.dontCheck super.wai-extra;
        wai-app-static = haskellLib.dontCheck super.wai-app-static;
        servant-client = haskellLib.dontCheck super.servant-client;

        # hw-hspec-hedgehog doesn't work
        # pact = haskellLib.dontCheck super.pact;

        unliftio = haskellLib.dontCheck super.unliftio;
        # prettyprinter-ansi-terminal = haskellLib.dontCheck (haskellLib.dontHaddock super.prettyprinter-ansi-terminal);
        # prettyprinter-convert-ansi-wl-pprint = haskellLib.dontCheck (haskellLib.dontHaddock super.prettyprinter-convert-ansi-wl-pprint);

        # servant-github = haskellLib.dontCheck super.servant-github;
      };
      common-overlay = self: super:
        let
          callHackageDirect = {pkg, ver, sha256}@args:
            let pkgver = "${pkg}-${ver}";
            in self.callCabal2nix pkg (pkgs.fetchzip {
                url = "http://hackage.haskell.org/package/${pkgver}/${pkgver}.tar.gz";
                inherit sha256;
              }) {};
        in {

        # nixpkgs has a bad hash for this
        # https://github.com/NixOS/nixpkgs/issues/65400
        servant = callHackageDirect {
          pkg = "servant";
          ver = "0.15";
          sha256 = "1d9bm5wgpk2czx240d7fd6k0mhhj52406gmj7vwpfcwrm7fg0w5v";
        };

        ghc-lib-parser = haskellLib.overrideCabal super.ghc-lib-parser { postInstall = "sed -i 's/exposed: True/exposed: False/' $out/lib/ghc*/package.conf.d/*.conf"; };
        pact = haskellLib.dontCheck super.pact; # TODO don't do this
        modern-uri = haskellLib.dontCheck super.modern-uri;
        servant-client-jsaddle = haskellLib.dontCheck (haskellLib.doJailbreak super.servant-client-jsaddle);
        obelisk-oauth-frontend = haskellLib.doJailbreak super.obelisk-oauth-frontend;
      };
    in self: super: lib.foldr lib.composeExtensions (_: _: {}) [
      (import (hackGet ./deps/pact + "/overrides.nix") pkgs)
      desktop-overlay
      common-overlay
      (optionalExtension (super.ghc.isGhcjs or false) guard-ghcjs-overlay)
      (optionalExtension (super.ghc.isGhcjs or false) ghcjs-overlay)
    ] self super;
  })
