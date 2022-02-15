{ nixosPkgs, hostName, enableHttps, ...}@args:
let
  system = "x86_64-linux";
  iosSdkVersion = "10.2";
  appRoot = import ./src/thunk.nix;
  kpkgs = import "${appRoot}/dep/kpkgs" { inherit system; };
  obelisk = import "${appRoot}/.obelisk/impl" { inherit system iosSdkVersion; inherit (kpkgs) reflex-platform-func;};
  pkgs = obelisk.reflex-platform.nixpkgs;
  obApp = import "${appRoot}/obApp.nix" { inherit system iosSdkVersion obelisk; };
  pactServerModule = import "${appRoot}/pact-server/service.nix";
in {...}: {
  imports = [
    (obelisk.serverModules.mkBaseEc2 args)
    # (nixosPkgs.path + /nixos/modules/virtualisation/virtualbox-image.nix)
    (pactServerModule {
      pactPort = 7010;
      nginxPort = 443;
      pactDataDir = "/var/lib/chainweaver";
      pactUser = "pact";
      location = "/pact/";
      z3 = kpkgs.pkgs.z3;
      inherit hostName enableHttps obApp pkgs;
    })
  ];
}
