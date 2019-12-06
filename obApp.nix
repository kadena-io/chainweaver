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
      ghcid = pkgs.haskell.lib.justStaticExecutables super.ghcid; #https://github.com/reflex-frp/reflex-platform/pull/548
     };
   packages =
     let
       servantSrc = hackGet ./deps/servant;
     in
       {
          pact = hackGet ./deps/pact;
          # servant-client-core = servantSrc + "/servant-client-core";
          # servant = servantSrc + "/servant";
          servant-jsaddle = servantSrc + "/servant-jsaddle";
          reflex-dom-ace = hackGet ./deps/reflex-dom-ace;
          reflex-dom-contrib = hackGet ./deps/reflex-dom-contrib;
          servant-github = hackGet ./deps/servant-github;
          obelisk-oauth-common = hackGet ./deps/obelisk-oauth + /common;
          obelisk-oauth-frontend = hackGet ./deps/obelisk-oauth + /frontend;
          obelisk-oauth-backend = hackGet ./deps/obelisk-oauth + /backend;

          # Needed for obelisk-oauth currently (ghcjs support mostly):
          entropy = hackGet ./deps/entropy;
          crc = hackGet ./deps/crc;
          cardano-crypto = hackGet ./deps/cardano-crypto;
          kadena-signing-api = hackGet ./deps/signing-api + /kadena-signing-api;
          desktop = ./desktop;
          mac = builtins.filterSource (path: type: !(builtins.elem (baseNameOf path) ["static"])) ./mac;
          linux = builtins.filterSource (path: type: !(builtins.elem (baseNameOf path) ["static"])) ./linux;
      };

    overrides = let
      inherit (pkgs) lib;

      mac-overlay = self: super: {
        # Mac app static linking
        mac = pkgs.haskell.lib.overrideCabal super.mac (drv: {
          preBuild = ''
            mkdir include
            ln -s ${pkgs.darwin.cf-private}/Library/Frameworks/CoreFoundation.framework/Headers include/CoreFoundation
            export NIX_CFLAGS_COMPILE="-I$PWD/include $NIX_CFLAGS_COMPILE"
          '';

          libraryFrameworkDepends =
            (with pkgs.darwin; with apple_sdk.frameworks; [
              Cocoa
              WebKit
            ]);

          configureFlags = [
            "--ghc-options=-optl=${(pkgs.openssl.override { static = true; }).out}/lib/libcrypto.a"
            "--ghc-options=-optl=${(pkgs.openssl.override { static = true; }).out}/lib/libssl.a"
            "--ghc-options=-optl=${pkgs.darwin.libiconv.override { enableShared = false; enableStatic = true; }}/lib/libiconv.a"
            "--ghc-options=-optl=${pkgs.zlib.static}/lib/libz.a"
            "--ghc-options=-optl=${pkgs.gmp6.override { withStatic = true; }}/lib/libgmp.a"
            "--ghc-options=-optl=/usr/lib/libSystem.dylib"
            "--ghc-options=-optl=${pkgs.libffi.override {
              stdenv = pkgs.stdenvAdapters.makeStaticLibraries pkgs.stdenv;
            }}/lib/libffi.a"
          ];
        });
      };
      addGObjectIntrospection = hpackage: pkgs.haskell.lib.overrideCabal hpackage (current: {
          libraryPkgconfigDepends =
            current.libraryPkgconfigDepends ++ [ pkgs.gobject-introspection ];
        });
      linux-overlay = self: super: {
        gi-gtk-hs = self.callHackageDirect {
          pkg = "gi-gtk-hs";
          ver =  "0.3.7.0";
          sha256 = "0h5959ayjvipj54z0f350bz23fic90xw9z06xw4wcvxvwkrsi2br";
        } { };
      };
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
        servant = haskellLib.dontCheck super.servant;
        servant-server = haskellLib.dontCheck super.servant-server;

        # failing
        mono-traversable = haskellLib.dontCheck super.mono-traversable;
        conduit = haskellLib.dontCheck super.conduit;
        wai-extra = haskellLib.dontCheck super.wai-extra;
        wai-app-static = haskellLib.dontCheck super.wai-app-static;
        servant-client = haskellLib.dontCheck super.servant-client;

        unliftio = haskellLib.dontCheck super.unliftio;
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
        ghc-lib-parser = haskellLib.overrideCabal super.ghc-lib-parser { postInstall = "sed -i 's/exposed: True/exposed: False/' $out/lib/ghc*/package.conf.d/*.conf"; };
        reflex-dom-core = haskellLib.dontCheck super.reflex-dom-core; # webdriver fails to build
        modern-uri = haskellLib.dontCheck super.modern-uri;
        servant-jsaddle = haskellLib.dontCheck (haskellLib.doJailbreak super.servant-jsaddle);
        these-lens = haskellLib.doJailbreak (self.callHackage "these-lens" "1" {});
        obelisk-oauth-frontend = haskellLib.doJailbreak super.obelisk-oauth-frontend;
        pact = haskellLib.dontCheck super.pact; # tests can timeout...
      };
    in self: super: lib.foldr lib.composeExtensions (_: _: {}) [
      (import (hackGet ./deps/pact + "/overrides.nix") pkgs hackGet)
      mac-overlay
      linux-overlay
      desktop-overlay
      common-overlay
      (optionalExtension (super.ghc.isGhcjs or false) guard-ghcjs-overlay)
      (optionalExtension (super.ghc.isGhcjs or false) ghcjs-overlay)
    ] self super;
  })
