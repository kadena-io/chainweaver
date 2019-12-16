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
  # If we don't wrapGApps, none of the gnome schema stuff works in nixos
  nixosLinuxApp = pkgs.haskell.lib.overrideCabal obApp.ghc.linux (drv: {
    libraryPkgconfigDepends =
      [ pkgs.webkitgtk
        pkgs.glib-networking
      ];
  });
  nixosExe = nixosLinuxApp.overrideAttrs (old: {
    nativeBuildInputs = old.nativeBuildInputs ++ [ pkgs.wrapGAppsHook ];
    libraryFrameworkDepends = [ pkgs.webkitgtk pkgs.glib-networking ];
    postInstall = ''
      set -eux
      mv $out/bin/linuxApp $out/bin/${linuxAppName}
      ln -s "${pkgs.z3}"/bin/z3 "$out/bin/z3"
      ln -s "${obApp.mkAssets obApp.passthru.staticFiles}" "$out/bin/static.assets"
      ln -s "${obApp.passthru.staticFiles}" "$out/bin/static"
      ln -s "${sass}/sass.css" "$out/bin/sass.css"
      ${pkgs.libicns}/bin/png2icns "$out/bin/pact.icns" "${macAppIcon}"
      ${pkgs.libicns}/bin/png2icns "$out/bin/pact-document.icns" "${macPactDocumentIcon}"
    '';
  });
  addGObjectIntrospection = hpackage: pkgs.haskell.lib.overrideCabal hpackage (current: {
    libraryPkgconfigDepends =
      current.libraryPkgconfigDepends ++ [ pkgs.gobject-introspection ];
  });
  ubuntuHPkgs = obApp.ghc.override {
    overrides = self: super: {
      gi-javascriptcore = addGObjectIntrospection (self.callHackage "gi-javascriptcore" "4.0.20" { webkitgtk = ubuntuWebkitgtk; });
      gi-webkit2 = addGObjectIntrospection (self.callHackage "gi-webkit2" "4.0.24" { webkitgtk = ubuntuWebkitgtk; });
      webkitgtk3-javascriptcore = addGObjectIntrospection (self.callHackage "webkitgtk3-javascriptcore" "0.14.2.1" { webkitgtk = ubuntuWebkitgtk; });
      linux = pkgs.haskell.lib.overrideCabal super.linux (old: {
        libraryPkgconfigDepends = [ ubuntuWebkitgtk pkgs.glib-networking ];
      });
    };
  };
  ubuntuExe = pkgs.haskell.lib.justStaticExecutables ubuntuHPkgs.linux;
  # Webkitgtk hard compiles the libexec path into the library, so we have to patch it if we
  # want it to work not from the nix store.
  ubuntuWebkitgtk = pkgs.webkitgtk.overrideAttrs (old: {
    patches = old.patches ++ [
      # Note this patch overrides the macro, because we can't do it in the cmake file lol
      # This wrecks anything we try to do to reference outside the nix store. 
      # https://github.com/NixOS/nixpkgs/blob/master/pkgs/development/tools/build-managers/cmake/setup-hook.sh#L10
      # Also in newer webgtks there is some sandboxing going on, which means that we may have to fiddle with another patch
      # when that happens. See https://github.com/NixOS/nixpkgs/commit/84fb39ef12a71caabd4fb4a87a4892497bcb2f7f
      ./patches/ubuntu/webkitgtk/ubuntupaths.patch
    ];
  });
  deb = pkgs.stdenv.mkDerivation {
    name = "${linuxAppName}-usrlib";
    outputs = ["out"];
    nativeBuildInputs = [ pkgs.makeWrapper ];
    exportReferencesGraph = ["graph" ubuntuExe];
    builder = pkgs.writeScript "builder.sh" ''
      source $stdenv/setup
      set -ex
      mkdir $out
      export LIBPATH=/usr/lib/chainweaver
      export LIBEXECPATH=/usr/libexec/chainweaver
      export BINPATH=/usr/bin
      export DEBDIR=$TMPDIR/${linuxAppName}-1
      export BINDIR=$DEBDIR$BINPATH
      export LIBEXECDIR=$DEBDIR$LIBEXECPATH
      export LIBDIR=$DEBDIR$LIBPATH
      export TMPEXE=$TMPDIR/${linuxAppName}
      export CHAINWEAVER_LIBEXEC_EXE=$LIBEXECDIR/${linuxAppName}
      export CHAINWEAVER_BIN_EXE=$BINDIR/${linuxAppName}

      # We want to patch a bunch of stuff to use the ubuntu things
      function chainweaver_patchelf () {
        patchelf --set-interpreter /lib64/ld-linux-x86-64.so.2 $1
        patchelf --set-rpath $LIBPATH $1
      }

      # make debian control file structure
      mkdir -p $DEBDIR/DEBIAN
      cp ${deb-control} $DEBDIR/DEBIAN/control
      cp ${deb-copyright} $DEBDIR/DEBIAN/copyright
      cp ${deb-changelog} $DEBDIR/DEBIAN/changelog

      mkdir -p $BINDIR
      mkdir -p $LIBDIR
      mkdir -p $LIBEXECDIR

      cp ${obApp.ghc.linux}/bin/linuxApp $TMPEXE
      chmod u+w $TMPEXE
       
      cp $TMPEXE $CHAINWEAVER_LIBEXEC_EXE
      cp "${pkgs.z3}"/bin/z3 "$LIBEXECDIR/z3"

      #TODO : This shouldn't be in libexec
      cp -rL "${obApp.mkAssets obApp.passthru.staticFiles}" "$LIBEXECDIR/static.assets"
      cp -rL "${obApp.passthru.staticFiles}" "$LIBEXECDIR/static"
      cp -rL "${sass}/sass.css" "$LIBEXECDIR/sass.css"

      # Copy the webgtk libexec over because we really want the webworker. 
      # This depends on our patch to ubuntuWebkitgtk for the lib to find the exec dir
      cp ${ubuntuWebkitgtk}/libexec/webkit2gtk-4.0/* $LIBEXECDIR/
      # We don't need the gi-repository folders in lib. They are a compile time gobject introspection
 
      # Figure out how to make the array work with nix escaping
      wrapperArgs=""
      wrapperArgs+="--set WEBKIT_DISABLE_COMPOSITING_MODE 1 "
      wrapperArgs+="--set LD_LIBRARY_PATH $LIBPATH "

      function copy_gio_modules() {
        path=$1
        dep_name=$(basename $path | cut -f2- -d'-')

        mkdir -p $LIBDIR/gio/$dep_name
        cp -L $path/lib/gio/modules/* $LIBDIR/gio/$dep_name
        wrapperArgs+="--prefix GIO_EXTRA_MODULES : $LIBPATH/gio/$dep_name "
      }

      # I can't get this in the graph for some silly reason
      copy_gio_modules ${pkgs.glib-networking}

      while read path; do
        if [ -d "$path/lib" ]; then
           for f in $(find $path/lib -maxdepth 1 -type f -name '*.so*'); do
            if [ ! -f "$LIBDIR/$(basename $f)" ]; then
               cp -L $f "$LIBDIR"
               so_name=$(basename $f)
 
               # If the so is a .so.MAJOR.MINOR.PATCH type file
               if echo $f | egrep "(\.[[:digit:]]+){2,}$"; then
                 trimmed_so=$LIBDIR/$(echo $so_name | sed -e 's/\(\.[[:digit:]]\+\)\(\.[[:digit:]]\+\)*$/\1/');
                 if [ ! -e "$trimmed_so" ]; then
                   ln -s "$LIBPATH/$so_name" $trimmed_so || true;
                 fi;
               fi;
            fi
          done;
          if [ -d "$path/lib/gio/modules" ]; then
            copy_gio_modules $path
          fi;
        fi;
        read dummy
        read nrRefs
        for ((i = 0; i < nrRefs; i++)); do read ref; done
      done < graph

      # libopenjp has some symlinks from libopenjp2.so -> libopenjp2.7. And we need them so hack it in
      ln -s $LIBPATH/libopenjp2.so.2.3.1 $LIBDIR/libopenjp2.so.7
      chmod 0755 $LIBEXECDIR/*
      for f in $(find $LIBEXECDIR -executable -type f); do
        if ${pkgs.file}/bin/file $f | egrep "ELF 64-bit LSB executable.+interpreter /nix/sto"; then
          chainweaver_patchelf $f
        fi
      done;
      if [ ! -z "$wrapperArgs" ]; then
        makeWrapper $CHAINWEAVER_LIBEXEC_EXE $CHAINWEAVER_BIN_EXE $wrapperArgs
        sed -i "s|$CHAINWEAVER_LIBEXEC_EXE|$LIBEXECPATH/${linuxAppName}|" $CHAINWEAVER_BIN_EXE
        sed -i "1s|#! /nix/store.*|#! /usr/bin/env bash|" $CHAINWEAVER_BIN_EXE
      fi
      
      ${pkgs.dpkg}/bin/dpkg-deb --build $DEBDIR $out

    '';
  };
  # This encodes the env vars that we need to wrap up to make the libexec exe happy
  deb-wrapperscript = pkgs.writeTextFile (linuxAppName + "-wrapper") ''
    export WEBKIT_DISABLE_COMPOSITING_MODE=1
    export LD_LIBRARY_PATH=/usr/lib/chainweaver
    # add ,interactive to get the gtk debugger up when you run this.
    export GTK_DEBUG=modules
    # Need to do stuff 
    /usr/libexec/chainweaver/${linuxAppName}
  '';
  deb-changelog = pkgs.writeTextFile { name = "chainweaver-changelog"; text = ''
    Nah
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
    Upstream-Name: Kiln
    Source: https://gitlab.com/obsidian.systems/kiln

    Files: *
    Copyright: 2019 Kadena.io
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
