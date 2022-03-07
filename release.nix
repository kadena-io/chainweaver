{ supportedSystems ? ["x86_64-linux" "x86_64-darwin"] }:
let pkgs = (import ./.obelisk/impl {}).nixpkgs;
    inherit (pkgs) lib;
    buildForPlatform = system:
      let self = import ./. { inherit system; };
          ci = self.ci;
      in lib.recurseIntoAttrs
        ({}
        // (if system == "x86_64-darwin" then ci.mac else {})
        // (if system == "x86_64-linux" then ci.linux else {}));
in lib.recurseIntoAttrs (lib.genAttrs supportedSystems buildForPlatform)
