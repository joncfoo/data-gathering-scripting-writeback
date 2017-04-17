{ghc}:
with (import <nixpkgs> {});

haskell.lib.buildStackProject {
  name = "default-stack-shell";
  inherit ghc;
  buildInputs = [ pkgconfig zlib ];
  LANG = "en_US.UTF-8";
}
