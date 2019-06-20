{ system ? builtins.currentSystem
, iosSdkVersion ? "10.2"
, obelisk ? (import ./.obelisk/impl { inherit system iosSdkVersion; })
, pkgs ? obelisk.reflex-platform.nixpkgs
}:
with obelisk;
let
  obApp = import ./obApp.nix { inherit system iosSdkVersion obelisk pkgs; };
  pactServerModule = import ./pact-server/service.nix;
  macAppName = "Pact";
  plist = pkgs.writeText "plist" ''
    <?xml version="1.0" encoding="UTF-8"?>
    <!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
    <plist version="1.0>
    <dict>
      <key>CFBundleName</key>
      <string>${macAppName}</string>
      <key>CFBundleDisplayName</key>
      <string>${macAppName}</string>
      <key>CFBundleIdentifier</key>
      <string>io.kadena.Pact</string>
      <key>CFBundleVersion</key>
      <string>${obApp.ghc.frontend.version}</string>
      <key>CFBundleShortVersionString</key>
      <string>${obApp.ghc.frontend.version}</string>
      <key>CFBundlePackageType</key>
      <string>APPL</string>
      <key>CFBundleSignature</key>
      <string>pact</string>
      <key>CFBundleExecutable</key>
      <string>${macAppName}</string>
      <key>NSHumanReadableCopyright</key>
      <string>(C) 2018 Kadena</string>

      <key>CFBundleIconFile</key>
      <string>pact.icns</string>

      <key>NSHighResolutionCapable</key>
      <string>YES</string>

      <!-- Handle pact:// addresses. This is necessary for OAuth redirection -->
      <key>CFBundleURLTypes</key>
      <array>
        <dict>
          <key>CFBundleURLName</key>
          <string>Pact Handler</string>
          <key>CFBundleURLSchemes</key>
          <array>
            <string>pact</string>
          </array>
        </dict>
      </array>

      <!-- Allow loading resources from the http backend -->
      <key>NSAppTransportSecurity</key>
      <dict>
        <key>NSExceptionDomains</key>
        <dict>
          <key>localhost</key>
          <dict>
            <key>NSExceptionAllowsInsecureHTTPLoads</key>
            <true/>
            <key>NSIncludesSubdomains</key>
            <true/>
          </dict>
        </dict>
      </dict>
    </dict>
    </plist>
  '';

in obApp // rec {
  sass = pkgs.runCommand "sass" {} ''
    set -eux
    mkdir $out
    ${pkgs.sass}/bin/sass ${./backend/sass}/index.scss $out/sass.css
  '';
  # Mac app static linking
  macBackend = pkgs.haskell.lib.overrideCabal obApp.ghc.backend (drv: {
    configureFlags = [
      "--ghc-options=-optl=${(pkgs.openssl.override { static = true; }).out}/lib/libcrypto.a"
      "--ghc-options=-optl=${(pkgs.openssl.override { static = true; }).out}/lib/libssl.a"
      "--ghc-options=-optl=${pkgs.libiconv.override { enableStatic = true; }}/lib/libiconv.a"
      "--ghc-options=-optl=${pkgs.zlib.static}/lib/libz.a"
      "--ghc-options=-optl=${pkgs.gmp6.override { withStatic = true; }}/lib/libgmp.a"
      "--ghc-options=-optl=/usr/lib/libSystem.B.dylib"
    ];
  });
  mac = pkgs.runCommand "mac" {} ''
    mkdir -p $out/${macAppName}.app/Contents
    mkdir -p $out/${macAppName}.app/Contents/MacOS
    mkdir -p $out/${macAppName}.app/Contents/Resources
    set -eux
    ln -s "${macBackend}"/bin/macApp $out/${macAppName}.app/Contents/MacOS/${macAppName}
    ln -s "${obApp.mkAssets obApp.passthru.staticFiles}" $out/${macAppName}.app/Contents/Resources/static.assets
    ln -s "${obApp.passthru.staticFiles}" $out/${macAppName}.app/Contents/Resources/static
    ln -s "${./mac/pact.icns}" $out/${macAppName}.app/Contents/Resources/pact.icns
    ln -s "${./mac/index.html}" $out/${macAppName}.app/Contents/Resources/index.html
    ln -s "${sass}/sass.css" $out/${macAppName}.app/Contents/Resources/sass.css
    cat ${plist} > $out/${macAppName}.app/Contents/Info.plist
  '';
  deployMac = pkgs.runCommand "deploy-mac" {} ''
    cp -Lr "${mac}" $out
  '';

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
          #   pactDataDir = "/var/lib/pact-web";
          #   pactUser = "pact";
          # })
        ];
        system.activationScripts = {
          setupBackendRuntime = {
            text = ''
                mkdir -p /var/lib/pact-web/dyn-configs
              '';
            deps = [];
          };
        };
      };
    };
}

