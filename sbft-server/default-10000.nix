{ system ? builtins.currentSystem # TODO: Get rid of this system cruft
, iosSdkVersion ? "10.2"
# , obelisk ? (import ../.obelisk/impl { inherit system iosSdkVersion; })
# , pkgs ? obelisk.reflex-platform.nixpkgs
, hostName
, adminEmail
}:
let
  # obApp = import ../obApp.nix { inherit system iosSdkVersion obelisk pkgs; };
  sbftServerModule = import ./service.nix;
  nixos = import (pkgs.path + /nixos);
  args = { inherit hostName adminEmail; routeHost = hostName; enableHttps = true;};
in
  nixos {
    system = "x86_64-linux";
    configuration = {
      imports = [
        # (obelisk.serverModules.mkBaseEc2 args)
        (sbftServerModule {
          myDbFile = "./log/node0-pact.sqlite";
          myApiPort = 9000;

          myAlias = "node0";
          myPort = 10000;
          myPublicKey = "54ea50ec9f2ec61d60ee194ca99ad2300eb8d7d94848957b67d0d74be8e08ae7";
          mySignerSecret = "c7e6a39bb01e3c664b9ccabc09881e879dff69522062aa268ff97d196ef3873b";
          mySignerPublic = "86dcf328701182c7ce3f1eac5615b3a250477bb6d676df96cb93896ad0b17022";
          myEphemeralSecret = "d93bcec62c773baf0e6759ef77067ad7a568b4c96e621bf8dff7cc9359abd6c0";
          myEphemeralPublic = "87fa14eaea9cc264d4a12658bdc58d0f7ba548d61b56583131671e43ff27f042";
          myStaticSecret = "cd8920b0d6cb6e9e0cbedc424cb149bd4b4f83268bc0c7a918b4689aa4eec955";
          myStaticPublic = "b85d5be9c694244a5288b9c02e8c15673762ab99debef5b81da6055fa4730000";
          myAdminKey = "e2b13b2dad4c843b071cc6bf04be671fc082f499f18c4381d57eacba8b47c2d4";
          myPrivateKey = "2f5a9e24b841cc6ccae50fa0d5b458f1a2d616bca1e32a48c467490e452bbf3f";


          alias-a = "node1";
          port-a = 10001;
          publicKey-a = "66e1b556db54a451d6923dd83de9fe46a3329528f1615fe0ee715ebd78c17d23";
          remote-static-a = "723ced973b7ce171e35866ef26ba9fbefbe032444d5c8b1ef797af46defe3701";
          remote-name-a = "Bob";

          alias-b = "node2";
          port-b = 10002;
          publicKey-b = "851d5bc11689055348e0ff0a1370c249c9d7bc627c43c394eaca649feaa7b4dc";
          remote-static-b = "ad0dcb8002fe64544dc147758d07a5cc9f98f70c60c9595f502c240dba79eb4b";
          remote-name-b = "Carol";

          alias-c = "node3";
          port-c = 10003";
          publicKey-c = "542f27a3f87d132f9dfa213150634096370c072a058f3a5b4e840da0703a23ad";
          remote-static-c = "3ca9b8d3822052a5d32fed41f4245ae62ddb2a0e7e7849f234b49a87f3d20f51";
          remote-name-c = "Dinesh";

          #inherit hostName obApp pkgs;
        })
      ];
      services.nginx.enable = true;
    };
  }
