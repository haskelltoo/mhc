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

  # The internal build function. Creates a derivation that builds a Haskell input.
  build = import ./build-haskell-internal.nix {
    inherit pkgs;
    inherit cabal-install;
    inherit ghc;
    inherit haskell;
  };

  # Build a single named Haskell component.
  buildOutput =
    input:
    {
      inherit (input)
        name;
      value = build input;
    };
  
  # Build all the requested Haskell components, and output them as an attribute set.
  outputs = lib.fix (
    self:
    listToAttrs (map buildOutput (inputs self))
  );

  builtDrvs = attrValues outputs;

  internalDepends =
    map (x: x.getCabalDeps) builtDrvs;

  isNotBuilt =
    x:
    lib.all (drv: x.outPath or null != drv.outPath) builtDrvs;

  combine = concatMap (filter isNotBuilt);

  externalDepends =
    zipAttrsWith (_: combine) internalDepends;
in
{
  inherit outputs;

  shell =
    let
      placeholderArgs = {
        pname = "shell";
        version = "0";
        license = null;
      };

      # Create a placeholder Haskell derivation.
      placeholder =
        haskell.mkDerivation (placeholderArgs // externalDepends);

      drv = placeholder.envFunc {};
    in
    drv.overrideAttrs (
      old:
      {
        nativeBuildInputs =
          old.nativeBuildInputs ++ extraTools haskell;
      }
    );
}
