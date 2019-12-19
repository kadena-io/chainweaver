{ system ? builtins.currentSystem }:
let self = import ./. {};
in {
  inherit (self) exe;
}
