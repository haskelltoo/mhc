let
  sources = import ./nix/sources.nix;
in

{
  nixpkgs ? sources.nixpkgs,
  system ? builtins.currentSystem,

  ghcVersion ? "ghc9101",

  ...
}@args:

let
  buildHaskell = import ./nix/build-haskell.nix;

  pkgs =
    if nixpkgs ? legacyPackages then
      nixpkgs.legacyPackages.${system}
    else
      import nixpkgs {
        inherit system;
      };
  
  haskell =
    if args ? haskell then
      args.haskell
    else
      pkgs.haskell.packages.${ghcVersion};
in rec
{
  hanjiru = buildHaskell {
    inherit haskell;

    name = "hanjiru";
    version = "0";
    src = ./code/hanjiru;

    license = pkgs.lib.licenses.bsd3;
  };

  mhc = buildHaskell {
    inherit haskell;

    name = "mhc";
    version = "0";
    src = ./code/mhc;

    license = pkgs.lib.licenses.bsd3;
  };

  mhc-haskell = buildHaskell {
    inherit haskell;

    name = "mhc-haskell";
    version = "0";
    src = ./code/mhc-haskell;

    license = pkgs.lib.licenses.bsd3;
  };

  develop = haskell.shellFor {
    packages = _: [
      hanjiru
      mhc
      mhc-haskell
    ];

    nativeBuildInputs = with haskell; [
      cabal-install
      haskell-language-server
    ];
  };

  packages = {
    default = mhc;
    inherit hanjiru;
    inherit mhc;
    inherit mhc-haskell;
  };

  devShells.default = develop;
}
