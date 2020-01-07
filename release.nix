{}:
let self = import ./. {};
    ci = self.ci;
in ci.cross // ci.mac // ci.linux
