args@{ system ? builtins.currentSystem
, iosSdkVersion ? "10.2"
, obelisk ? (import ./.obelisk/impl { inherit system iosSdkVersion; })
, pkgs ? obelisk.reflex-platform.nixpkgs
, withHoogle ? false
}:
with obelisk;
let
  obApp = import ./obApp.nix args;
  pactServerModule = import ./pact-server/service.nix;
  macAppName = "Kadena Chainweaver Beta";
  macAppIcon =  ./mac/static/icons/kadena.png;
  linuxAppName = "kadena-chainweaver-rc1";
  macPactDocumentIcon = ./mac/static/icons/pact-document.png;
  # ^ This can be created in Preview using 'GenericDocumentIcon.icns' from
  # /System/Library/CoreServices/CoreTypes.bundle/Contents/Resources/
  # and the kadena logo
  macAppInstallerBackground = ./mac/static/installer-background.png;
  bundleIdentifier = "io.kadena.chainweaver";
  createDmg = pkgs.fetchFromGitHub {
    owner = "andreyvit";
    repo = "create-dmg";
    rev = "395bc0de23cd3499e0c6d0d1bafdbf4b074d5516";
    sha256 = "046x566m352mgr9mh8p8iyhr6b71di10m8f36zibaiixa0ca3cr0";
  };

  xcent = builtins.toFile "xcent" (nixpkgs.lib.generators.toPlist {} {
    "com.apple.security.app-sandbox" = false; # TODO enable this
    "com.apple.security.network.client" = true;
    "com.apple.security.network.server" = true;
  });

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
      <string>${bundleIdentifier}</string>
      <key>CFBundleVersion</key>
      <string>${obApp.ghc.frontend.version}</string>
      <key>CFBundleShortVersionString</key>
      <string>${obApp.ghc.frontend.version}</string>
      <key>CFBundlePackageType</key>
      <string>APPL</string>
      <key>CFBundleExecutable</key>
      <string>${macAppName}</string>
      <key>NSHumanReadableCopyright</key>
      <string>(C) 2018 Kadena</string>
      <key>CFBundleIconFile</key>
      <string>pact.icns</string>
      <key>NSHighResolutionCapable</key>
      <string>YES</string>

      <!-- Associate .pact files with the app -->
      <key>CFBundleDocumentTypes</key>
      <array>
        <dict>
          <key>CFBundleTypeExtensions</key>
          <array>
            <string>pact</string>
          </array>
          <key>CFBundleTypeIconFile</key>
          <string>pact-document.icns</string>
          <key>CFBundleTypeRole</key>
          <string>Editor</string>
          <key>CFBundleTypeName</key>
          <string>Pact Smart Contract</string>
          <key>LSHandlerRank</key>
          <string>Owner</string>
        </dict>
      </array>

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
        <key>NSAllowsLocalNetworking</key>
        <true/>
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
  # Linux app static linking
  linuxNixos = pkgs.haskell.lib.overrideCabal obApp.ghc.linux (drv: {
    nativeBuildInputs = old.nativeBuildInputs ++ [ pkgs.wrapGAppsHook ];
    libraryFrameworkDepends =
      [ pkgs.webkit
        pkgs.glib-networking
      ];
  });
  linuxApp = pkgs.stdenv.mkDerivation {
    name = "${linuxAppName}-usrlib";
    outputs = ["out"];
    
    builder = pkgs.writeScript "builder.sh" ''
      source $stdenv/setup
      set -eux
      mkdir $out
      export DEBDIR=$TMPDIR/${linuxAppName}-1
      export BINDIR=$DEBDIR/usr/libexec/chainweaver
      export LIBDIR=$DEBDIR/usr/lib/chainweaver
      export TMPEXE=$TMPDIR/${linuxAppName}

      # make debian control file structure
      mkdir -p $DEBDIR/DEBIAN
      cp ${deb-control} $DEBDIR/DEBIAN/control
      cp ${deb-copyright} $DEBDIR/DEBIAN/copyright
      cp ${deb-changelog} $DEBDIR/DEBIAN/changelog

      mkdir -p $BINDIR
      mkdir -p $LIBDIR

      cp ${obApp.ghc.linux}/bin/linuxApp $TMPEXE
      chmod u+w $TMPEXE
      patchelf --set-interpreter /lib64/ld-linux-x86-64.so.2 $TMPEXE
      patchelf --set-rpath $LIBDIR $TMPEXE
      cp $TMPEXE $BINDIR/${linuxAppName}

      cp "${pkgs.z3}"/bin/z3 "$BINDIR/z3"
      #TODO : This shouldn't be in libexec
      cp -rL "${obApp.mkAssets obApp.passthru.staticFiles}" "$BINDIR/static.assets"
      cp -rL "${obApp.passthru.staticFiles}" "$BINDIR/static"
      cp -rL "${sass}/sass.css" "$BINDIR/sass.css"
      ${pkgs.libicns}/bin/png2icns "$BINDIR/pact.icns" "${macAppIcon}"
      ${pkgs.libicns}/bin/png2icns "$BINDIR/pact-document.icns" "${macPactDocumentIcon}"
      cp $(ldd ${obApp.ghc.linux}/bin/linuxApp | awk '{ print $3; }') $LIBDIR/
      chmod 0755 $BINDIR/*
      ${pkgs.dpkg}/bin/dpkg-deb --build $DEBDIR $out
    '';
  };
  deb-changelog = pkgs.writeTextFile { name = "chainweaver-changelog"; text = ''
    TODO
  ''; };
  deb-control = pkgs.writeTextFile { name = "control"; text = ''
    Package: chainweaver
    Version: 0.1.0
    Architecture: amd64
    Maintainer: "Kadena"
    Description: "Chainweaver"
  ''; };

  deb-copyright = pkgs.writeTextFile { name = "chainweaver-deb-copyright"; text = ''
    Format: https://www.debian.org/doc/packaging-manuals/copyright-format/1.0/
    Upstream-Name: Chainweaver
    Source: https://gitlab.com/kadena.io/chainweaver

    Files: *
    Copyright: 2019 kadena.io
    License: MIT
      Permission is hereby granted, free of charge, to any person obtaining a copy
      of this software and associated documentation files (the "Software"), to deal
      in the Software without restriction, including without limitation the rights
      to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
      copies of the Software, and to permit persons to whom the Software is
      furnished to do so, subject to the following conditions:

      The above copyright notice and this permission notice shall be included in all
      copies or substantial portions of the Software.

      THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
      IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
      FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
      AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
      LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
      OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
      SOFTWARE.
  ''; };

  # Use native mac libc++
  fixedZ3 = pkgs.z3.overrideAttrs (oldAttrs: rec {
    fixupPhase = ''
      install_name_tool -change \
        "${pkgs.libcxx}/lib/libc++.1.0.dylib" \
        /usr/bin/libc++.dylib \
        "$out/bin/z3"
    '';
  });
  mac = pkgs.runCommand "mac" {} ''
    mkdir -p "$out/${macAppName}.app/Contents"
    mkdir -p "$out/${macAppName}.app/Contents/MacOS"
    mkdir -p "$out/${macAppName}.app/Contents/Resources"
    set -eux
    # Copy instead of symlink, so we can set the path to z3
    cp "${obApp.ghc.mac}"/bin/macApp "$out/${macAppName}.app/Contents/MacOS/${macAppName}"
    ln -s "${fixedZ3}"/bin/z3 "$out/${macAppName}.app/Contents/MacOS/z3"
    ln -s "${obApp.mkAssets obApp.passthru.staticFiles}" "$out/${macAppName}.app/Contents/Resources/static.assets"
    ln -s "${obApp.passthru.staticFiles}" "$out/${macAppName}.app/Contents/Resources/static"
    ${pkgs.libicns}/bin/png2icns "$out/${macAppName}.app/Contents/Resources/pact.icns" "${macAppIcon}"
    ${pkgs.libicns}/bin/png2icns "$out/${macAppName}.app/Contents/Resources/pact-document.icns" "${macPactDocumentIcon}"
    ln -s "${./mac/static/index.html}" "$out/${macAppName}.app/Contents/Resources/index.html"
    ln -s "${sass}/sass.css" "$out/${macAppName}.app/Contents/Resources/sass.css"
    cat ${plist} > "$out/${macAppName}.app/Contents/Info.plist"
  '';
  deployMac = pkgs.writeScript "deploy" ''
    #!/usr/bin/env bash
    set -eo pipefail

    if (( "$#" < 1 )); then
      echo "Usage: $0 [TEAM_ID]" >&2
      exit 1
    fi

    TEAM_ID=$1
    shift

    set -euox pipefail

    function cleanup {
      if [ -n "$tmpdir" -a -d "$tmpdir" ]; then
        echo "Cleaning up tmpdir" >&2
        chmod -R +w $tmpdir
        rm -fR $tmpdir
      fi
    }

    trap cleanup EXIT

    tmpdir=$(mktemp -d)
    # Find the signer given the OU
    signer=$(security find-certificate -c "Mac Developer" -a \
      | grep '^    "alis"<blob>="' \
      | sed 's|    "alis"<blob>="\(.*\)"$|\1|' \
      | while read c; do \
          security find-certificate -c "$c" -p \
            | openssl x509 -subject -noout; \
        done \
      | grep "OU=$TEAM_ID/" \
      | sed 's|subject= /UID=[^/]*/CN=\([^/]*\).*|\1|' \
      | head -n 1)

    if [ -z "$signer" ]; then
      echo "Error: No Mac Developer certificate found for team id $TEAM_ID" >&2
      echo "See https://github.com/kadena-io/chainweaver/blob/redesign/README.md#mac-developer-certificate" >&2
      exit 1
    fi

    # Create and sign the app
    mkdir -p "$tmpdir"
    cp -LR "${mac}/${macAppName}.app" "$tmpdir"
    chmod -R +w "$tmpdir/${macAppName}.app"
    strip "$tmpdir/${macAppName}.app/Contents/MacOS/${macAppName}"
    sed "s|<team-id/>|$TEAM_ID|" < "${xcent}" > "$tmpdir/xcent"
    cat "$tmpdir/xcent"
    plutil "$tmpdir/xcent"
    /usr/bin/codesign --deep --force --sign "$signer" --entitlements "$tmpdir/xcent" --timestamp=none "$tmpdir/${macAppName}.app"

    # Create the dmg
    ${createDmg}/create-dmg \
      --volname "${macAppName} Installer" \
      --background "${macAppInstallerBackground}" \
      --window-pos 200 120 \
      --window-size 800 400 \
      --icon-size 100 \
      --icon "${macAppName}.app" 200 190 \
      --app-drop-link 600 185 \
      "$tmpdir/${macAppName}.dmg" \
      "$tmpdir/${macAppName}.app"

    # Sign the dmg
    /usr/bin/codesign --sign "$signer" "$tmpdir/${macAppName}.dmg"

    mv "$tmpdir/${macAppName}.dmg" .

    # Quarantine it for reproducibility (otherwise can cause unexpected 'app is damaged' errors when automatically applied to downloaded .dmg files)
    xattr -w com.apple.quarantine "00a3;5d4331e1;Safari;1AE3D17F-B83D-4ADA-94EA-219A44467959" "${macAppName}.dmg"
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
}
