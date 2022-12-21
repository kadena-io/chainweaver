# DO NOT HAND-EDIT THIS FILE
let fetch = { private ? false, fetchSubmodules ? false, owner, repo, rev, sha256, ... }:
  if !fetchSubmodules && !private then builtins.fetchTarball {
    url = "https://github.com/${owner}/${repo}/archive/${rev}.tar.gz"; inherit sha256;
  } else (import <nixpkgs> {}).fetchFromGitHub {
    inherit owner repo rev sha256 fetchSubmodules private;
  };
  json =
    builtins.fromJSON
      (builtins.readFile
         (if (builtins.currentSystem == "x86_64-darwin") ||
             (builtins.currentSystem == "aarch64-darwin")
          then ./github-mac.json
          else ./github.json));
in fetch json
