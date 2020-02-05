{ pkgs, nixosExe, appName, linuxAppName, chainweaverVersion, ovaReleaseNumber, nixosDesktopItem, homeManagerModule, linuxAppIcon }:
let
  kadenaOVACache = "https://nixcache.chainweb.com";
  kadenaOVACacheKey = "nixcache.chainweb.com:FVN503ABX9F8x8K0ptnc99XEz5SaA4Sks6kNcZn2pBY=";
  desktopItemPath = "${nixosDesktopItem}/share/applications/${linuxAppName}.desktop";
  doUpgradeVM = let
    resultStorePathFile = "https://chainweaver-builds.s3.amazonaws.com/release-store-paths/latest-ova";
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
      finished "Successfully upgraded to $(${linuxAppName}-version)";
    }

    echo "Currently on chainweaver OVA version $(${linuxAppName}-version)";
    (${doUpgradeVM}/bin/${linuxAppName}-do-upgrade $@ && successful) || finished "Update Failed"
  '';
  versionFile = pkgs.writeText "${linuxAppName}-version" "${chainweaverVersion}.${ovaReleaseNumber}";
  versionScript = pkgs.writeScriptBin "${linuxAppName}-version" ''cat ${versionFile}'';
  upgradeDesktopItem = pkgs.makeDesktopItem {
     name = "${linuxAppName}-upgrade";
     desktopName = "Upgrade ${appName}";
     exec = "${upgradeVM}/bin/${linuxAppName}-upgrade";
     icon = linuxAppIcon;
     terminal = "true";
  };
  chainweaverVMConfig = (import ((import ./deps/ovanixpkgs {}).pkgs.path + /nixos) {
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
      home-manager.users.chainweaver = { config, ... }: {
        home.file.".config/xfce4/helpers.rc".source = ./ova/home/chainweaver/config/xfce4/helpers.rc;
        home.file."desktop.png".source = ./ova/home/chainweaver/desktop.png;
        home.file."Desktop/${linuxAppName}.desktop".source = desktopItemPath;
        home.file."Desktop/${linuxAppName}-upgrade.desktop".source = "${upgradeDesktopItem}/share/applications/${linuxAppName}-upgrade.desktop";
        # Instead of symlinking these and them potentially conflicting on upgrades, we'll
        # copy them each time our profile activates. This may override something that the user
        # fiddles with, but it should hopefully be OK.
        home.activation.checkoutOrg = config.lib.dag.entryAfter [ "writeBoundary" ] ''
            FOLDER="$HOME/.config/xfce4/xfconf/xfce-perchannel-xml";
            mkdir -p $FOLDER
            for i in $(find ${./ova/home/chainweaver/config/xfce4/xfconf/xfce-perchannel-xml} -type f); do
              cp $i $FOLDER
            done;
            chmod u+w -R $FOLDER
          '';
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
        systemd.user.services.kadena-chainweaver = {
          Unit = {
            Description = appName;
            After = [ "network-online.target" ];
            Wants = [ "network-online.target" ];
          };
          Service = {
            Environment = "WEBKIT_DISABLE_COMPOSITING_MODE=1";
            ExecStart = "${nixosExe}/bin/${linuxAppName}";
          };
          Install = { WantedBy = [ "graphical-session.target" ]; };
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
      networking.firewall.enable = false;
      networking.networkmanager.enable = true;
      environment.systemPackages = [ nixosExe pkgs.chromium nixosDesktopItem upgradeVM versionScript upgradeDesktopItem ];
      nix.binaryCaches = [ "https://cache.nixos.org/" "https://nixcache.reflex-frp.org" kadenaOVACache ];
      nix.binaryCachePublicKeys = [ "ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI=" kadenaOVACacheKey ];

      nixpkgs = { localSystem.system = "x86_64-linux"; };
      virtualbox = {
        baseImageSize = 32  * 1024; # in MiB
        memorySize = 2 * 1024; # in MiB
        vmDerivationName = "${linuxAppName}-vm";
        vmName = "${appName} VM";
        vmFileName = "${linuxAppName}-vm.${chainweaverVersion}.${ovaReleaseNumber}.ova";
      };
    };
  });
in {
  chainweaverVM = chainweaverVMConfig.config.system.build.virtualBoxOVA;
  chainweaverVMSystem = chainweaverVMConfig.system;
}
