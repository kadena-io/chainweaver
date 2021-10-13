{ obApp
, pkgs
, appName
, chainweaverVersion
, linuxReleaseNumber
, ovaReleaseNumber
, sass
, homeManagerModule
, patched-certs ? ./certs/ca-certificates-patched.crt
, linuxAppName ? "kadena-chainweaver"
, linuxAppIcon ? ./linux/static/icons/pact-document.png
}: rec {
  # If we don't wrapGApps, none of the gnome schema stuff works in nixos
  nixosLinuxApp = pkgs.haskell.lib.justStaticExecutables (pkgs.haskell.lib.overrideCabal obApp.ghc.linux (drv: {
    libraryPkgconfigDepends =
      [ pkgs.webkitgtk
        pkgs.glib-networking
      ];
  }));
  nixosExe = nixosLinuxApp.overrideAttrs (old: {
    nativeBuildInputs = old.nativeBuildInputs ++ [ pkgs.wrapGAppsHook ];
    libraryFrameworkDepends = [ pkgs.webkitgtk pkgs.glib-networking ];
    postInstall = ''
      set -eux
      mv $out/bin/linuxApp $out/bin/${linuxAppName}
      ln -s "${pkgs.z3}"/bin/z3 "$out/bin/z3"
      ln -s "${obApp.obelisk.mkAssets obApp.passthru.staticFiles}" "$out/bin/static.assets"
      ln -s "${obApp.passthru.staticFiles}" "$out/bin/static"
      ln -s "${sass}/sass.css" "$out/bin/sass.css"
      ln -s "${patched-certs}" "$out/bin/ca-certificates-patched.crt"
    '';
  });
  nixosWrapper = pkgs.writeScriptBin "${linuxAppName}-wrapper" ''
    #!/usr/bin/env bash
    WEBKIT_DISABLE_COMPOSITING_MODE=1 NIX_SSL_CERT_FILE=${nixosExe}/bin/ca-certificates-patched.crt ${nixosExe}/bin/${linuxAppName}
  '';
  nixosDesktopItem = pkgs.makeDesktopItem {
     name = linuxAppName;
     desktopName = appName;
     exec = "${nixosWrapper}/bin/${linuxAppName}-wrapper";
     icon = linuxAppIcon;
  };
  ova = import ./ova.nix { inherit pkgs nixosExe appName linuxAppName chainweaverVersion ovaReleaseNumber nixosDesktopItem homeManagerModule linuxAppIcon; };
  inherit (ova) chainweaverVM chainweaverVMSystem;

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
    nativeBuildInputs = [ pkgs.makeWrapper pkgs.libcanberra ];
    exportReferencesGraph = ["graph" ubuntuExe];
    builder = pkgs.writeScript "builder.sh" ''
      source $stdenv/setup
      set -ex
      mkdir $out
      export PREFIX=/usr
      export LIBPATH=$PREFIX/lib/chainweaver
      export LIBEXECPATH=$PREFIX/libexec/chainweaver
      export BINPATH=$PREFIX/bin
      export SHAREPATH=$PREFIX/share/chainweaver

      export DEBDIR=$TMPDIR/${linuxAppName}-1
      export BINDIR=$DEBDIR$BINPATH
      export LIBEXECDIR=$DEBDIR$LIBEXECPATH
      export LIBDIR=$DEBDIR$LIBPATH
      export SHAREDIR=$DEBDIR$SHAREPATH
      export APPLICATIONSDIR=$DEBDIR$PREFIX/share/applications
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
      mkdir -p $SHAREDIR
      mkdir -p $APPLICATIONSDIR

      cp ${obApp.ghc.linux}/bin/linuxApp $TMPEXE
      chmod u+w $TMPEXE

      cp $TMPEXE $CHAINWEAVER_LIBEXEC_EXE
      cp "${pkgs.z3}"/bin/z3 "$LIBEXECDIR/z3"

      #TODO : This shouldn't be in libexec
      cp -rL "${obApp.obelisk.mkAssets obApp.passthru.staticFiles}" "$SHAREDIR/static.assets"
      cp -rL "${obApp.passthru.staticFiles}" "$SHAREDIR/static"
      cp -rL "${sass}/sass.css" "$SHAREDIR/sass.css"

      cp ${linuxAppIcon} "$SHAREDIR/icon.png"
      cp ${linuxDesktopItem}/share/applications/${linuxAppName}.desktop $APPLICATIONSDIR

      # Copy the webgtk libexec over because we really want the webworker.
      # This depends on our patch to ubuntuWebkitgtk for the lib to find the exec dir
      cp ${ubuntuWebkitgtk}/libexec/webkit2gtk-4.0/* $LIBEXECDIR/
      # We don't need the gi-repository folders in lib. They are a compile time gobject introspection

      # Figure out how to make the array work with nix escaping
      wrapperArgs=""
      wrapperArgs+="--set WEBKIT_DISABLE_COMPOSITING_MODE 1 "
      wrapperArgs+="--set LD_LIBRARY_PATH $LIBPATH "
      wrapperArgs+="--set CHAINWEAVER_STATIC_PATH $SHAREPATH "

      function copy_gio_modules() {
        path=$1
        dep_name=$(basename $path | cut -f2- -d'-')

        if [ -d "$path/lib/gio/modules" ]; then
          mkdir -p $LIBDIR/gio/$dep_name
          cp -L $path/lib/gio/modules/* $LIBDIR/gio/$dep_name
          wrapperArgs+="--prefix GIO_EXTRA_MODULES : $LIBPATH/gio/$dep_name "
        fi
      }

      mkdir -p $SHAREDIR/gsettings-schemas
      function copy_gtk_settings() {
        path=$1
        gsettings_path=$path/share/gsettings-schemas

        if [ -d $gsettings_path ]; then
          cp -r $gsettings_path/. $SHAREDIR/gsettings-schemas/
          for d in $(find $gsettings_path -maxdepth 1 -type d); do
            wrapperArgs+="--prefix XDG_DATA_DIRS : $SHAREDIR/gsettings-schemas/$(basename $d) "
          done;
        fi
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
          copy_gio_modules $path
          copy_gtk_settings $path;
        fi;
        read dummy
        read nrRefs
        for ((i = 0; i < nrRefs; i++)); do read ref; done
      done < graph

      # The pixbuf api is critically important for rendering images to the low level gnome drawing tools. Because it links
      # back to glibc, it's very important to make sure it's the same libc and stack as we built gtk with.
      pixbuf_path=${pkgs.gdk_pixbuf}
      cp -r $pixbuf_path/lib/gdk-pixbuf-2.0 $LIBDIR/gdk-pixbuf-2.0
      pixbuf_nixcache=$(ls ${pkgs.gdk_pixbuf}/lib/gdk-pixbuf-2.0/*/loaders.cache)
      pixbuf_filename=$(echo $pixbuf_nixcache | sed "s|$pixbuf_path/lib||")

      # The pixbuf dir isn't writable by root, so temporarily make it so to replace the cache config
      # alternatively we could use sponge or copy the file somewhere else, but this is
      chmod u+w $LIBDIR/gdk-pixbuf-2.0/2.10.0
      sed -i "s|$pixbuf_path/lib|$LIBPATH|g" $LIBDIR$pixbuf_filename
      chmod u-w $LIBDIR/gdk-pixbuf-2.0/2.10.0
      wrapperArgs+="--set GDK_PIXBUF_MODULE_FILE $LIBPATH$pixbuf_filename "

      # Copy gio-launch-desktop across and setup glib to use it
      cp ${pkgs.glib}/bin/gio-launch-desktop $LIBEXECDIR
      wrapperArgs+="--set GIO_LAUNCH_DESKTOP $LIBEXECPATH/gio-launch-desktop "

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
  deb-changelog = pkgs.writeTextFile { name = "chainweaver-changelog"; text = ''
    kadena-chainweaver (0.1.0-1) unstable; urgency=medium

    * Initial release

    -- Obsidian Systems Thu, 9 Jan 2020 02:09:09 +1000
  ''; };
  linuxDesktopItem = pkgs.makeDesktopItem {
     name = linuxAppName;
     desktopName = appName;
     exec = "/usr/bin/${linuxAppName}";
     icon= "/usr/share/chainweaver/icon.png";
  };
  deb-control = pkgs.writeTextFile { name = "control"; text = ''
    Package: ${linuxAppName}
    Version: ${chainweaverVersion}.${linuxReleaseNumber}
    Architecture: amd64
    Maintainer: "Kadena.io"
    Description: ${appName}
  ''; };

  deb-copyright = pkgs.writeTextFile { name = "chainweaver-deb-copyright"; text = ''
    Format: https://www.debian.org/doc/packaging-manuals/copyright-format/1.0/
    Upstream-Name: ${appName}
    Source: https://github.com/kadena-io/chainweaver

    Files: *
    Copyright: 2020 Kadena
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
}
