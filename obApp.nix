{ system ? builtins.currentSystem # TODO: Get rid of this system cruft
, iosSdkVersion ? "10.2"
, withHoogle ? false
, kpkgs ? import ./dep/kpkgs { inherit system; }
, obelisk
}:
let
  pkgs = obelisk.reflex-platform.nixpkgs;

  optionalExtension = cond: overlay: if cond then overlay else _: _: {};
  getGhcVersion = ghc: if ghc.isGhcjs or false then ghc.ghcVersion else ghc.version;
  haskellLib = pkgs.haskell.lib;
in with obelisk;
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
       servantSrc = hackGet ./dep/servant;
       reflex-dom-src = hackGet ./dep/reflex-dom;
     in
       {
          # servant-client-core = servantSrc + "/servant-client-core";
          # servant = servantSrc + "/servant";
          servant-jsaddle = hackGet ./dep/servant-jsaddle;
          jsaddle-warp = hackGet ./dep/jsaddle + /jsaddle-warp; #https://github.com/ghcjs/jsaddle/pull/114
          kadena-signing-api = hackGet ./dep/signing-api + "/kadena-signing-api";
          pact = hackGet ./dep/pact;
          reflex-dom = reflex-dom-src + "/reflex-dom";
          reflex-dom-core = reflex-dom-src + "/reflex-dom-core";
          reflex-dom-ace = hackGet ./dep/reflex-dom-ace;
          reflex-dom-contrib = hackGet ./dep/reflex-dom-contrib;
          dependent-sum-aeson-orphans = hackGet ./dep/dependent-sum-aeson-orphans;
          servant-github = hackGet ./dep/servant-github;
          obelisk-oauth-common = hackGet ./dep/obelisk-oauth + /common;
          obelisk-oauth-frontend = hackGet ./dep/obelisk-oauth + /frontend;
          obelisk-oauth-backend = hackGet ./dep/obelisk-oauth + /backend;
          HsYAML-aeson = hackGet ./dep/HsYAML-aeson;

          # Needed for obelisk-oauth currently (ghcjs support mostly):
          entropy = hackGet ./dep/entropy;
          crc = hackGet ./dep/crc;
          cardano-crypto = hackGet ./dep/cardano-crypto;
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

          # Hiding the static libs from the dylibs seems to be the only way i am able
          # to statically link against libcrypto
          preConfigure = let openssl-static = (pkgs.openssl.override { static = true; }).out; in ''
            mkdir -p openssl-static-libs
            cp ${openssl-static}/lib/libcrypto.a openssl-static-libs
            cp ${openssl-static}/lib/libssl.a openssl-static-libs
          '';
            
          configureFlags = [
            "--ghc-option=-optl=-Lopenssl-static-libs"
            "--ghc-option=-optl=-lcrypto"
            "--ghc-option=-optl=-lssl"
            "--ghc-option=-optl=${pkgs.darwin.libiconv.override { enableShared = false; enableStatic = true; }}/lib/libiconv.a"
            "--ghc-option=-optl=${pkgs.zlib.static}/lib/libz.a"
            "--ghc-option=-optl=${pkgs.gmp6.override { withStatic = true; }}/lib/libgmp.a"
            "--ghc-option=-optl=/usr/lib/libSystem.dylib"
            "--ghc-option=-optl=${pkgs.libffi.override {
              stdenv = pkgs.stdenvAdapters.makeStaticLibraries pkgs.stdenv;
            }}/lib/libffi.a"
          ];
        });
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

        # failing
        mono-traversable = haskellLib.dontCheck super.mono-traversable; #https://github.com/ghcjs/ghcjs-base/issues/128
        conduit = haskellLib.dontCheck super.conduit;
        unliftio = haskellLib.dontCheck super.unliftio;
      };
      common-overlay = self: super: {
        brittany = haskellLib.dontCheck super.brittany;
        haskell-gi-base =
          haskellLib.addPkgconfigDepend
            (haskellLib.dontCheck (self.callHackage "haskell-gi-base" "0.24.5" {}))
            pkgs.gobject-introspection;
        haskell-gi =
            (haskellLib.dontCheck (self.callHackage "haskell-gi" "0.24.7" {}));
        gi-cairo =
          haskellLib.addPkgconfigDepends
            (haskellLib.dontCheck (self.callHackage "gi-cairo" "1.0.24" {}))
            [pkgs.cairo pkgs.gobject-introspection];
        gi-glib =
          haskellLib.addPkgconfigDepends
            (haskellLib.dontCheck (self.callHackage "gi-glib" "2.0.24" {}))
            [pkgs.gobject-introspection];
        gi-gobject =
          haskellLib.addPkgconfigDepends
            (haskellLib.dontCheck (self.callHackage "gi-gobject" "2.0.24" {}))
            [pkgs.gobject-introspection];
        gi-atk =
          haskellLib.addPkgconfigDepends
            (haskellLib.dontCheck (self.callHackage "gi-atk" "2.0.22" {}))
            [pkgs.gobject-introspection];
        gi-pango =
          haskellLib.addPkgconfigDepends
            (haskellLib.dontCheck (self.callHackage "gi-pango" "1.0.23" {}))
            [pkgs.gobject-introspection pkgs.harfbuzz];
        gi-harfbuzz =
          haskellLib.addBuildDepends
            (haskellLib.addPkgconfigDepends
              (haskellLib.dontCheck (self.callHackage "gi-harfbuzz" "0.0.3" {}))
              [pkgs.gobject-introspection pkgs.harfbuzz])
             [pkgs.harfbuzz-gobject];
        jsaddle-warp = haskellLib.dontCheck super.jsaddle-warp; # webdriver fails to build
        reflex-dom-contrib = haskellLib.doJailbreak (haskellLib.dontCheck super.reflex-dom-contrib); # webdriver fails to build
        reflex-dom-core = haskellLib.dontCheck super.reflex-dom-core; # webdriver fails to build
        servant-jsaddle = haskellLib.dontCheck (haskellLib.doJailbreak super.servant-jsaddle);
        semialign = haskellLib.doJailbreak super.semialign; # vector bounds
        these-lens = haskellLib.doJailbreak super.these-lens; # lens bounds
        pact = haskellLib.dontCheck super.pact; # tests can timeout...
        system-locale = haskellLib.dontCheck super.system-locale; # tests fail on minor discrepancies on successfully parsed locale time formats.
        typed-process = haskellLib.dontCheck super.typed-process;
        pact-time = haskellLib.dontCheck (self.callHackageDirect {
          pkg = "pact-time";
          ver = "0.2.0.0";
          sha256 = "1cfn74j6dr4279bil9k0n1wff074sdlz6g1haqyyy38wm5mdd7mr";
        } {});
        direct-sqlite = dontCheck (self.callHackageDirect {
          pkg = "direct-sqlite";
          ver = "2.3.26";
          sha256 = "1kdkisj534nv5r0m3gmxy2iqgsn6y1dd881x77a87ynkx1glxfva";
        } {});

      };
    in self: super: lib.foldr lib.composeExtensions (_: _: {}) [
      mac-overlay
      common-overlay
      (optionalExtension (super.ghc.isGhcjs or false) guard-ghcjs-overlay)
      (optionalExtension (super.ghc.isGhcjs or false) ghcjs-overlay)
    ] self super;
  })
