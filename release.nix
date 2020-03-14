{ system ? builtins.currentSystem }:
let self = import ./. {};
    ci = self.ci;
in
  {}
  // (if system == "x86_64-darwin" then ci.mac else {})
  // (if system == "x86_64-linux" then ci.linux else {})
