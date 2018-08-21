{ system ? builtins.currentSystem # TODO: Get rid of this system cruft
, iosSdkVersion ? "10.2"
}:
with import ./.obelisk/impl { inherit system iosSdkVersion; };
project ./. ({ pkgs, ... }: {
  android.applicationId = "systems.obsidian.obelisk.examples.minimal";
  android.displayName = "Obelisk Minimal Example";
  ios.bundleIdentifier = "systems.obsidian.obelisk.examples.minimal";
  ios.bundleName = "Obelisk Minimal Example";

  overrides = self: super:
    let guardGhcjs = p: if self.ghc.isGhcjs or false then null else p;

        semantic-reflex-src = pkgs.fetchFromGitHub {
          owner = "tomsmalley";
          repo = "semantic-reflex";
          rev = "38fce7e4d08d46b8664768f1b7fe38846dbac1e2";
          sha256 = "1s2p12r682wd8j2z63pjvbi4s9v02crh6nz8kjilwdsfs02yp5p2";
        };
     in {
          blake2 = guardGhcjs super.blake2;
          cacophony = guardGhcjs (pkgs.haskell.lib.dontCheck (self.callHackage "cacophony" "0.8.0" {}));
          cryptonite = guardGhcjs (self.callHackage "cryptonite" "0.23" {});
          haskeline = guardGhcjs (self.callHackage "haskeline" "0.7.4.2" {});
          katip = guardGhcjs (pkgs.haskell.lib.doJailbreak (self.callHackage "katip" "0.3.1.4" {}));
          ridley = guardGhcjs (pkgs.haskell.lib.dontCheck (self.callHackage "ridley" "0.3.1.2" {}));

          # Needed to work with the below version of statistics
          criterion = guardGhcjs (pkgs.haskell.lib.dontCheck (self.callCabal2nix "criterion" (pkgs.fetchFromGitHub {
            owner = "bos";
            repo = "criterion";
            rev = "5a704392b670c189475649c32d05eeca9370d340";
            sha256 = "1kp0l78l14w0mmva1gs9g30zdfjx4jkl5avl6a3vbww3q50if8pv";
          }) {}));

          # Version 1.6.4, needed by weeder, not in callHackage yet
          extra = guardGhcjs (pkgs.haskell.lib.dontCheck (self.callCabal2nix "extra" (pkgs.fetchFromGitHub {
            owner = "ndmitchell";
            repo = "extra";
            rev = "4064bfa7e48a7f1b79f791560d51dbefed879219";
            sha256 = "1p7rc5m70rkm1ma8gnihfwyxysr0n3wxk8ijhp6qjnqp5zwifhhn";
          }) {}));

          pact = pkgs.haskell.lib.dontCheck (self.callCabal2nix "pact" (pkgs.fetchFromGitHub {
            owner = "kadena-io";
            repo = "pact";
            rev = "8f4216d274749507158c5e23be8c4b32a75191a6";
            sha256 = "13rpmjyr1x5z0yfxzr16ml1nmx6ln6bs9xwlzh5c6c6kkqwxpkjv";
          }) {});

          reflex-dom-ace = (self.callCabal2nix "reflex-dom-ace" (pkgs.fetchFromGitHub {
            owner = "reflex-frp";
            repo = "reflex-dom-ace";
            rev = "24e1ee4b84f50bd5b6b4401c4bdc28963ce8d80f";
            sha256 = "0hdn00cd17a7zp56krqs3y5mpcml75pn8mnmhwyixqgscqd1q9y5";
          }) {});

          # sbv >= 7.6
          sbv = pkgs.haskell.lib.dontCheck (self.callCabal2nix "sbv" (pkgs.fetchFromGitHub {
            owner = "LeventErkok";
            repo = "sbv";
            rev = "dbbdd396d069dc8235f5c8cf58209886318f6525";
            sha256 = "0s607qbgiykgqv2b5sxcvzqpj1alxzqw6szcjzhs4hxcbbwkd60y";
          }) {});

          # dontCheck is here because a couple tests were failing
          statistics = guardGhcjs (pkgs.haskell.lib.dontCheck (self.callCabal2nix "statistics" (pkgs.fetchFromGitHub {
            owner = "bos";
            repo = "statistics";
            rev = "1ed1f2844c5a2209f5ea72e60df7d14d3bb7ac1a";
            sha256 = "1jjmdhfn198pfl3k5c4826xddskqkfsxyw6l5nmwrc8ibhhnxl7p";
          }) {}));

          thyme = pkgs.haskell.lib.dontCheck (pkgs.haskell.lib.enableCabalFlag (self.callCabal2nix "thyme" (pkgs.fetchFromGitHub {
            owner = "kadena-io";
            repo = "thyme";
            rev = "6ee9fcb026ebdb49b810802a981d166680d867c9";
            sha256 = "09fcf896bs6i71qhj5w6qbwllkv3gywnn5wfsdrcm0w1y6h8i88f";
          }) {}) "ghcjs");

          semantic-reflex = pkgs.haskell.lib.dontCheck (self.callCabal2nix "semantic-reflex"  (semantic-reflex-src + /semantic-reflex) {});

        };
})
