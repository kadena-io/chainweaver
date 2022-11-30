{ system ? builtins.currentSystem # TODO: Get rid of this system cruft
, iosSdkVersion ? "10.2"
, withHoogle ? false
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
    externjs = ./static/externs.js;
    staticFiles =
      let
        staticFilePath = ./static;
      in
      pkgs.runCommand "minify" { buildInputs = [ pkgs.xorg.lndir ]; }
        ''
        mkdir -p $out
        cd $out
        lndir -silent ${staticFilePath} $out
        '${pkgs.closurecompiler}/bin/closure-compiler' --externs 'externs.js' --externs '${reflex-platform.ghcjsExternsJs}' --language_in=ECMASCRIPT_2018 --jscomp_warning=checkVars --js_output_file="js/kadena-crypto.min.js" "js/kadena-crypto.js"
        '';
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
          attoparsec = hackGet ./dep/attoparsec;
          # Playing with attempts at fixing the xhr spin problem
          # jsaddle = hackGet ./dep/jsaddle + /jsaddle;
          hashable = hackGet ./dep/hashable; #https://github.com/ghcjs/jsaddle/pull/114
          jsaddle-warp = hackGet ./dep/jsaddle + "/jsaddle-warp"; #https://github.com/ghcjs/jsaddle/pull/114
          kadena-signing-api = hackGet ./dep/signing-api + "/kadena-signing-api";
          pact = hackGet ./dep/pact;
          reflex-dom = reflex-dom-src + "/reflex-dom";
          reflex-dom-core = reflex-dom-src + "/reflex-dom-core";
          reflex-dom-ace = hackGet ./dep/reflex-dom-ace;
          reflex-dom-contrib = hackGet ./dep/reflex-dom-contrib;
          dependent-sum-aeson-orphans = hackGet ./dep/dependent-sum-aeson-orphans;
          servant-github = hackGet ./dep/servant-github;
          obelisk-oauth-common = hackGet ./dep/obelisk-oauth + "/common";
          obelisk-oauth-frontend = hackGet ./dep/obelisk-oauth + "/frontend";
          obelisk-oauth-backend = hackGet ./dep/obelisk-oauth + "/backend";
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
      linux-overlay = self: super: {
        gi-gtk-hs = self.callHackageDirect {
          pkg = "gi-gtk-hs";
          ver =  "0.3.9";
          sha256 = "1703r430c330a4vw6ifwjm2977381zv7xrvwdhfcd5fvfkkxd6gp";
        } { };
        # haskell-gi-base = self.callHackageDirect {
        #   pkg = "haskell-gi-base";
        #   ver =  "0.26.3";
        #   sha256 = "1dvc8p458lsbmyfvid9szbblr1w0drv2m2f2dfx55qqxc2ypikw6";
        # } { };
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
        jsaddle-warp = haskellLib.doJailbreak (haskellLib.dontCheck super.jsaddle-warp); # webdriver fails to build
        hashable = haskellLib.dontCheck super.hashable;
        reflex-dom-contrib = haskellLib.doJailbreak (haskellLib.dontCheck super.reflex-dom-contrib); # webdriver fails to build
        reflex-dom-core = haskellLib.dontCheck super.reflex-dom-core; # webdriver fails to build
        servant-jsaddle = haskellLib.dontCheck (haskellLib.doJailbreak super.servant-jsaddle);
        attoparsec = haskellLib.dontCheck (haskellLib.doJailbreak super.attoparsec);
        semialign = haskellLib.doJailbreak super.semialign; # vector bounds
        these-lens = haskellLib.doJailbreak super.these-lens; # lens bounds
        pact = haskellLib.doJailbreak (haskellLib.dontCheck super.pact); # tests can timeout...
        system-locale = haskellLib.dontCheck super.system-locale; # tests fail on minor discrepancies on successfully parsed locale time formats.
        typed-process = haskellLib.dontCheck super.typed-process;
        pact-time = haskellLib.dontCheck (self.callHackageDirect {
          pkg = "pact-time";
          ver = "0.2.0.0";
          sha256 = "1cfn74j6dr4279bil9k0n1wff074sdlz6g1haqyyy38wm5mdd7mr";
        } {});
        direct-sqlite = dontCheck (self.callHackageDirect {
          pkg = "direct-sqlite";
          ver = "2.3.27";
          sha256 = "0w8wj3210h08qlws40qhidkscgsil3635zk83kdlj929rbd8khip";
        } {});
        deriving-compat = doJailbreak (dontCheck (self.callHackageDirect {
          pkg = "deriving-compat";
          ver = "0.5.9";
          sha256 = "0xppvj420plp4s4h7v02y3msiyx7pz9z36w1i0v7gb5sx9zd9yc3";
        } {}));
        invariant = doJailbreak (dontCheck (self.callHackageDirect {
          pkg = "invariant";
          ver = "0.5.3";
          sha256 = "0fcgpwb9f9hx8p1803zigfy5c5baq1piw940jr9w0d9vhjwwxnk8";
        } {}));
        th-abstraction = dontCheck (self.callHackageDirect {
          pkg = "th-abstraction";
          ver = "0.4.5.0";
          sha256 = "19nh7a9b4yif6sijp6xns6xlxcr1mcyrqx3cfbp5bdm7mkbda7a9";
        } {});
        unordered-containers = dontCheck (self.callHackageDirect {
          pkg = "unordered-containers";
          ver = "0.2.17.0";
          sha256 = "0xlf97r7yvsyaxj13kwdz6np1j05hpfq4cdw7gmgx6ibh9lsibbm";
        } {}); # {
          #   nothunks = self.callHackageDirect {
          #     pkg = "nothunks";
          #     ver = "0.1.3";
          #     sha256 = "1xxxxxxxxxxxxxxxxxxns6xlxcr1mcyrqx3cfbp5bdm7mkbda7a9";
          #   };
          # });
        bifunctors = doJailbreak (dontCheck (self.callHackageDirect {
          pkg = "bifunctors";
          ver = "5.5.7";
          sha256 = "088wyh1nvcrs6mwqm6hh0lzi6c1wr6wh2a6ifpnaqnyll7jmf1xx";
        } {}));
        http-media = doJailbreak (dontCheck (self.callHackageDirect {
          pkg = "http-media";
          ver = "0.8.0.0";
          sha256 = "080xkljq1iq0i8wagg8kbzbp523p2awa98wpn9i4ph1dq8y8346y";
        } {}));
        dependent-sum-template = doJailbreak (dontCheck (self.callHackageDirect {
          pkg = "dependent-sum-template";
          ver = "0.1.0.3";
          sha256 = "0m5nblmwbx2810hhnlcz1c8vwis47kd3xir1ylfk0dgxa0n1ag3f";
        } {}));
        aeson-gadt-th = dontCheck (self.callHackageDirect {
          pkg = "aeson-gadt-th";
          ver = "0.2.5.1";
          sha256 = "1r3gx226jqs7l5jp8gmgaa2p49lnsnlzdhsxj6h47m0rnfc36qm5";
        } {});
        sbv = dontCheck (self.callHackageDirect {
          pkg = "sbv";
          ver = "9.0";
          sha256 = "14g2qax1vc7q4g78fa562dviqvcd0l52kd5jmgv90g3g3ci15bnl";
        } {});
        tasty = doJailbreak (dontCheck (self.callHackageDirect {
          pkg = "tasty";
          ver = "1.2.3";
          sha256 = "0l9ndn9mw8sizprxs840p9n3b9gb49i849jm1idc8c6584bzpsja";
        } {}));
        # sbv requires this even though it is not used in the build (and the hash is invalid)
        tasty-bench = dontCheck (self.callHackageDirect {
          pkg = "tasty-bench";
          ver = "0.3.1";
          sha256 = "0000000000000000000000000000000000000000000000000000";
        } {});

        libBF = doJailbreak (dontCheck (self.callHackageDirect {
          pkg = "libBF";
          ver = "0.6.3";
          sha256 = "0j0i39jb389rnrkkw2xqz10471afxys79nf31hhlqr4fk6ddhjf7";
        } {}));

      };
    in self: super: lib.foldr lib.composeExtensions (_: _: {}) [
      mac-overlay
      linux-overlay
      common-overlay
      (optionalExtension (super.ghc.isGhcjs or false) guard-ghcjs-overlay)
      (optionalExtension (super.ghc.isGhcjs or false) ghcjs-overlay)
    ] self super;
  })
