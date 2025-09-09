{
  inputs ? []
, ghcVersion ? "ghc9101"
, pkgs
, cabal-install ? pkgs.cabal-install
, ghc ? pkgs.ghc
, haskell ? pkgs.haskell.packages.${ghcVersion}
}:

let
  build = import ./build-haskell-internal.nix {
    inherit pkgs;
    inherit cabal-install;
    inherit ghc;
    inherit haskell;
  };

  buildNamed = args: {
    name = args.name;
    value = build args;
  };
in
builtins.listToAttrs (builtins.map buildNamed inputs)
