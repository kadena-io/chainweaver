{ obApp
, pkgs
, appName
, chainweaverVersion
, macReleaseNumber
, sass
, macAppIcon ? ./mac/static/icons/kadena.png
, macPactDocumentIcon ? ./mac/static/icons/pact-document.png
}:
let
  macPactDocumentIcon = ./mac/static/icons/pact-document.png;
  macFullVersion = "${chainweaverVersion}.${macReleaseNumber}";
  # ^ This can be created in Preview using 'GenericDocumentIcon.icns' from
  # /System/Library/CoreServices/CoreTypes.bundle/Contents/Resources/
  # and the kadena logo
  macAppInstallerBackground = ./mac/static/installer-background.png;
  bundleIdentifier = "io.kadena.chainweaver";
  createDmg = pkgs.fetchFromGitHub {
    owner = "andreyvit";
    repo = "create-dmg";
    rev = "fbe0f36c823adbcbdcc15f9d65c6354252ac8307";
    sha256 = "052vhzg0naq1ifyp21fjm7ga3611750wnxym9077y3j07zdkkp66";
  };

  xcent = builtins.toFile "xcent" (pkgs.lib.generators.toPlist {} {
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
      <string>${appName}</string>
      <key>CFBundleDisplayName</key>
      <string>${appName}</string>
      <key>CFBundleIdentifier</key>
      <string>${bundleIdentifier}</string>
      <key>CFBundleVersion</key>
      <string>${macFullVersion}</string>
      <key>CFBundleShortVersionString</key>
      <string>${macFullVersion}</string>
      <key>CFBundlePackageType</key>
      <string>APPL</string>
      <key>CFBundleExecutable</key>
      <string>${appName}</string>
      <key>NSHumanReadableCopyright</key>
      <string>(C) 2020 Kadena</string>
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
  # Use native mac libc++
in rec {
  fixedZ3 = pkgs.z3.overrideAttrs (oldAttrs: rec {
    fixupPhase = ''
      install_name_tool -change \
        "${pkgs.libcxx}/lib/libc++.1.0.dylib" \
        /usr/bin/libc++.dylib \
        "$out/bin/z3"
    '';
  });
  mac = pkgs.runCommand "mac" {} ''
    mkdir -p "$out/${appName}.app/Contents"
    mkdir -p "$out/${appName}.app/Contents/MacOS"
    mkdir -p "$out/${appName}.app/Contents/Resources"
    set -eux
    # Copy instead of symlink, so we can set the path to z3
    cp "${obApp.ghc.mac}"/bin/macApp "$out/${appName}.app/Contents/MacOS/${appName}"
    ln -s "${fixedZ3}"/bin/z3 "$out/${appName}.app/Contents/MacOS/z3"
    ln -s "${obApp.obelisk.mkAssets obApp.passthru.staticFiles}" "$out/${appName}.app/Contents/Resources/static.assets"
    ln -s "${obApp.passthru.staticFiles}" "$out/${appName}.app/Contents/Resources/static"
    ${pkgs.libicns}/bin/png2icns "$out/${appName}.app/Contents/Resources/pact.icns" "${macAppIcon}"
    ${pkgs.libicns}/bin/png2icns "$out/${appName}.app/Contents/Resources/pact-document.icns" "${macPactDocumentIcon}"
    ln -s "${./mac/static/index.html}" "$out/${appName}.app/Contents/Resources/index.html"
    ln -s "${sass}/sass.css" "$out/${appName}.app/Contents/Resources/sass.css"
    cat ${plist} > "$out/${appName}.app/Contents/Info.plist"
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
    signer=$(security find-certificate -c "Apple Distribution" -a \
      | grep '^    "alis"<blob>="' \
      | sed 's|    "alis"<blob>="\(.*\)"$|\1|' \
      | while read c; do \
          security find-certificate -c "$c" -p \
            | openssl x509 -subject -noout; \
        done \
      | grep "OU[[:space:]]\?=[[:space:]]\?$TEAM_ID" \
      | sed 's|subject= /UID=[^/]*/CN=\([^/]*\).*|\1|' \
      | head -n 1)
    if [ -z "$signer" ]; then
      echo "Error: No Apple Distribution certificate found for team id $TEAM_ID" >&2
      echo "See https://github.com/kadena-io/chainweaver/blob/redesign/README.md#mac-developer-certificate" >&2
      exit 1
    fi
    # Create and sign the app
    mkdir -p "$tmpdir"
    cp -LR "${mac}/${appName}.app" "$tmpdir"
    chmod -R +w "$tmpdir/${appName}.app"
    strip "$tmpdir/${appName}.app/Contents/MacOS/${appName}"
    sed "s|<team-id/>|$TEAM_ID|" < "${xcent}" > "$tmpdir/xcent"
    cat "$tmpdir/xcent"
    plutil "$tmpdir/xcent"
    /usr/bin/codesign --deep --force --sign "$signer" --entitlements "$tmpdir/xcent" --timestamp=none "$tmpdir/${appName}.app"

    # Create the dmg
    ${createDmg}/create-dmg \
      --no-internet-enable \
      --volname "${appName} Installer" \
      --background "${macAppInstallerBackground}" \
      --window-pos 200 120 \
      --window-size 800 400 \
      --icon-size 100 \
      --icon "${appName}.app" 200 190 \
      --app-drop-link 600 185 \
      "$tmpdir/${appName}.${macFullVersion}.dmg" \
      "$tmpdir/${appName}.app"
    # Sign the dmg
    /usr/bin/codesign --sign "$signer" "$tmpdir/${appName}.${macFullVersion}.dmg"
    mv "$tmpdir/${appName}.${macFullVersion}.dmg" .
    # Quarantine it for reproducibility (otherwise can cause unexpected 'app is damaged' errors when automatically applied to downloaded .dmg files)
    xattr -w com.apple.quarantine "00a3;5d4331e1;Safari;1AE3D17F-B83D-4ADA-94EA-219A44467959" "${appName}.${macFullVersion}.dmg"
  '';}
