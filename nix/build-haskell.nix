{
  inputs ? _self: []

, pkgs
, lib ? pkgs.lib
, stdenv ? pkgs.stdenv

, ghcVersion ? "ghc9101"
, ghc ? pkgs.ghc
, haskell ? pkgs.haskell.packages.${ghcVersion}

, cabal-install ? haskell.cabal-install

, extraTools ? _: []
}:

let
  inherit (builtins)
    attrValues
    concat
    concatMap
    filter
    listToAttrs
    map
    mapAttrs
    zipAttrsWith;

  build = import ./build-haskell-internal.nix {
    inherit pkgs;
    inherit cabal-install;
    inherit ghc;
    inherit haskell;
  };

  buildOutput =
    input:
    {
      inherit (input)
        name;
      value = build input;
    };
  
  outputs = lib.fix (
    self:
    listToAttrs (map buildOutput (inputs self))
  );
in
{
  inherit outputs;

  develop =
    let
      selected = attrValues outputs;

      isNotSelected = x: lib.all (y: x.outPath or null != y.outPath) selected;
      combine = concatMap (filter isNotSelected);

      depsForSelected = map (x: x.getCabalDeps) selected;
      depsCombined = zipAttrsWith (_: combine) depsForSelected;

      # Create a placeholder Haskell derivation.
      placeholder = haskell.mkDerivation (
        {
          pname = "shell";
          version = "0";
          license = null;
        } // depsCombined
      );

      drv = placeholder.envFunc {};
    in
    args:
    drv.overrideAttrs (
      old:
      args
      // {
        nativeBuildInputs =
          old.nativeBuildInputs ++
          extraTools haskell ++
          args.nativeBuildInputs or [];

        buildInputs =
          old.buildInputs ++
          args.builtInputs or [];
      }
    );
}
