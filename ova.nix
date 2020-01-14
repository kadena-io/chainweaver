{ pkgs, nixosExe }:
rec {
  chainweaverVMConfig =  {
    imports = [
      ./virtualbox-image.nix
    ];
    users.users.chainweaver = {
      isNormalUser = true;
      description = "Chainweaver account";
      extraGroups = [ "wheel" ];
      password = "";
      uid = 1000;
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
    environment.systemPackages = [ nixosExe pkgs.chromium ];
    nix.binaryCaches = [ "https://cache.nixos.org/" "https://nixcache.reflex-frp.org" ];
    nix.binaryCachePublicKeys = [ "ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI=" ];

    nixpkgs = { localSystem.system = "x86_64-linux"; };
    virtualbox = {
      baseImageSize = 32  * 1024; # in MiB
      memorySize = 2 * 1024; # in MiB
      vmDerivationName = "chainweaver-vm";
      vmName = "Chainweaver VM";
      vmFileName = "chainweaver-vm.ova";
    };
  };
  chainweaverVM = (import "${pkgs.path}/nixos/lib/eval-config.nix" {
    modules = [ chainweaverVMConfig ];
  }).config.system.build.virtualBoxOVA;
}
