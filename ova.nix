{ pkgs, nixosExe, appName, linuxAppName, chainweaverVersion, ovaReleaseNumber, nixosDesktopItem, homeManagerModule, linuxAppIcon }:
let
  kadenaOVACache = "https://nixcache.chainweb.com";
  kadenaOVACacheKey = "nixcache.chainweb.com:FVN503ABX9F8x8K0ptnc99XEz5SaA4Sks6kNcZn2pBY=";
  desktopItemPath = "${nixosDesktopItem}/share/applications/${linuxAppName}.desktop";
  doUpgradeVM = let
    resultStorePathFile = "https://chainweaver-builds.s3.amazonaws.com/vm/master-store-path";
  in pkgs.writeScriptBin "${linuxAppName}-do-upgrade" ''
    #!/usr/bin/env bash
    set -e
    CHAINWEAVER_CACHE_URL=''${CHAINWEAVER_CACHE_URL:-${kadenaOVACache}}
    if [[ $# -eq 0 ]] ; then
       echo "Downloading latest Chainweaver VM path"
       echo "Fetching ${resultStorePathFile}"
       export CHAINWEAVER_VM_STORE_PATH=`curl '${resultStorePathFile}'`
    else
       echo "Using the user supplied store path: $1"
       export CHAINWEAVER_VM_STORE_PATH=$1
    fi
    echo "Downloading Chainweaver"
    nix copy --from $CHAINWEAVER_CACHE_URL $CHAINWEAVER_VM_STORE_PATH
    echo "Installing Chainweaver"
    sudo nix-env -p /nix/var/nix/profiles/system --set $CHAINWEAVER_VM_STORE_PATH
    sudo /nix/var/nix/profiles/system/bin/switch-to-configuration switch
  '';
  upgradeVM = pkgs.writeScriptBin "${linuxAppName}-upgrade" ''
    #!/usr/bin/env bash
    function finished() {
      echo $1;
      echo "Press any key to continue";
      read;
    }
    function successful() {
      finished "Successfully upgraded to $(${linuxAppName}-version.sh)";
    }

    (${doUpgradeVM}/bin/${linuxAppName}-do-upgrade $@ && successful) || finished "Update Failed"
  '';
  versionFile = pkgs.writeText "${linuxAppName}-version" "${chainweaverVersion}.${ovaReleaseNumber}";
  versionScript = pkgs.writeScriptBin "${linuxAppName}-version.sh" ''cat ${versionFile}'';
  upgradeDesktopItem = pkgs.makeDesktopItem {
     name = "${linuxAppName}-upgrade";
     desktopName = "Upgrade ${appName}";
     exec = "${upgradeVM}/bin/${linuxAppName}-upgrade";
     icon = linuxAppIcon;
     terminal = "true";
  };
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
        home.file.".config/autostart/${linuxAppName}.desktop".source = desktopItemPath;
        home.file."desktop.png".source = ./ova/home/chainweaver/desktop.png;
        home.file."Desktop/${linuxAppName}.desktop".source = desktopItemPath;
        home.file."Desktop/${linuxAppName}-upgrade.desktop".source = "${upgradeDesktopItem}/share/applications/${linuxAppName}-upgrade.desktop";
        home.sessionVariables = {
          WEBKIT_DISABLE_COMPOSITING_MODE = 1;
        };
        programs.bash = {
          enable = true;
          sessionVariables = {
            WEBKIT_DISABLE_COMPOSITING_MODE = 1;
          };
          initExtra = ''
            export WEBKIT_DISABLE_COMPOSITING_MODE=1;
          '';
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
      security.sudo.wheelNeedsPassword = false;
      networking.firewall.allowedTCPPorts = [ 9467 ];
      environment.systemPackages = [ nixosExe pkgs.chromium nixosDesktopItem upgradeVM versionScript upgradeDesktopItem ];
      nix.binaryCaches = [ "https://cache.nixos.org/" "https://nixcache.reflex-frp.org" kadenaOVACache ];
      nix.binaryCachePublicKeys = [ "ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI=" kadenaOVACacheKey ];

      nixpkgs = { localSystem.system = "x86_64-linux"; };
      virtualbox = {
        baseImageSize = 32  * 1024; # in MiB
        memorySize = 2 * 1024; # in MiB
        vmDerivationName = "${linuxAppName}-vm";
        # This should not be Appname as this name will persist forever in the users virtualbox.
        vmName = "Kadena Chainweaver VM";
        vmFileName = "${linuxAppName}-vm.${chainweaverVersion}.${ovaReleaseNumber}.ova";
      };
    };
  });
in {
  chainweaverVM = chainweaverVMConfig.config.system.build.virtualBoxOVA;
  chainweaverVMSystem = chainweaverVMConfig.system;
}
