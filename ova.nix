{ pkgs, nixosExe, linuxAppName, nixosDesktopItem, homeManagerModule }:
let
  upgradeVM = let
    resultStorePathFile = "https://chainweaver-builds.s3.amazonaws.com/vm/master-store-path";
  in pkgs.writeScriptBin "upgrade-chainweaver" ''
    #!/usr/bin/env bash
    set -e
    if [[ $# -eq 0 ]] ; then
       echo "Downloading latest Chainweaver VM path"
       echo "Fetching ${resultStorePathFile}"
       export CHAINWEAVER_VM_STORE_PATH=`curl '${resultStorePathFile}'`
    else
       echo "Using the user supplied store path: $1"
       export CHAINWEAVER_VM_STORE_PATH=$1
    fi
    echo "Downloading Chainweaver"
    nix copy --from 'http://nixcache.kadena.io' $CHAINWEAVER_VM_STORE_PATH
    echo "Installing Chainweaver"
    sudo nix-env -p /nix/var/nix/profiles/system --set $CHAINWEAVER_VM_STORE_PATH
    sudo /nix/var/nix/profiles/system/bin/switch-to-configuration switch
  '';
  chainweaverVMConfig = (import (pkgs.path + /nixos) {
    configuration = {
      imports = [
        ./virtualbox-image.nix
        homeManagerModule
      ];
      users.users.chainweaver = {
        isNormalUser = true;
        description = "Chainweaver account";
        extraGroups = [ "wheel" ];
        password = "";
        uid = 1000;
      };
      home-manager.users.chainweaver = { ... }: {
        home.file.".config" = { source = ./ova/home/chainweaver/config; recursive = true; };
        home.file."desktop.png".source = ./ova/home/chainweaver/desktop.png;
        home.file."Desktop/${linuxAppName}.desktop".source = "${nixosDesktopItem}/share/applications/${linuxAppName}.desktop";
        home.sessionVariables = {
          WEBKIT_DISABLE_COMPOSITING_MODE = 1;
        };
      };
      services.xserver = {
        enable = true;
        displayManager.sddm.enable = true;
        displayManager.sddm.autoLogin = {
          enable = true;
          relogin = true;
          user = "chainweaver";
        };
        desktopManager.xfce.enable = true;
        libinput.enable = true; # for touchpad support on many laptops
      };
      networking.firewall.allowedTCPPorts = [ 9467 ];
      environment.systemPackages = [ nixosExe pkgs.chromium nixosDesktopItem upgradeVM ];
      nix.binaryCaches = [ "https://cache.nixos.org/" "https://nixcache.reflex-frp.org" "http://nixcache.kadena.io" ];
      nix.binaryCachePublicKeys = [ "ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI=" "kadena-cache.local-1:8wj8JW8V9tmc5bgNNyPM18DYNA1ws3X/MChXh1AQy/Q=" ];

      nixpkgs = { localSystem.system = "x86_64-linux"; };
      virtualbox = {
        baseImageSize = 32  * 1024; # in MiB
        memorySize = 2 * 1024; # in MiB
        vmDerivationName = "chainweaver-vm";
        vmName = "Chainweaver VM";
        vmFileName = "chainweaver-vm.ova";
      };
    };
  });
in {
  chainweaverVM = chainweaverVMConfig.config.system.build.virtualBoxOVA;
  chainweaverVMSystem = chainweaverVMConfig.system;
}
