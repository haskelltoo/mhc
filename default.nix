let
  sources = import ./nix/sources.nix;
in

{
  nixpkgs ? sources.nixpkgs
, system ? builtins.currentSystem
, ghcVersion ? "ghc9101"
}:

let
  pkgs =
    if nixpkgs ? legacyPackages then
      nixpkgs.legacyPackages.${system}
    else
      import nixpkgs {
        inherit system;
      };

  buildHaskell = import ./nix/build-haskell.nix;

  outputs = buildHaskell {
    inputs = [
      {
        name = "hanjiru";
        version = "0";
        src = ./code/hanjiru;

        buildInputs = hackage: with hackage; [
          base
          containers
          text
          transformers
        ];

        license = licenses.bsd3;
      }
      {
        name = "haskell-like";
        version = "0";
        src = ./code/haskell-like;

        buildInputs = hackage: with hackage; [
          base
          containers
          parsec
          prettyprinter
          text
          transformers
        ];

        license = licenses.bsd3;
      }
      {
        name = "mhc";
        version = "0";
        src = ./code/mhc;

        buildInputs = hackage: with hackage; [
          base
          outputs.hanjiru
          outputs.mhc-haskell
          text
        ];

        license = licenses.bsd3;
      }
      {
        name = "mhc-haskell";
        version = "0";
        src = ./code/mhc-haskell;

        buildInputs = hackage: with hackage; [
          base
          outputs.hanjiru
          text
        ];

        license = licenses.bsd3;
      }
    ];

    inherit ghcVersion;
    inherit pkgs;
    inherit haskell;
  };

  haskell = pkgs.haskell.packages.${ghcVersion};

  inherit (pkgs.lib) licenses;

  develop = haskell.shellFor {
    packages = _: [
      outputs.hanjiru
      outputs.haskell-like
      outputs.mhc
      outputs.mhc-haskell
    ];

    nativeBuildInputs = with haskell; [
      cabal-install
      haskell-language-server
    ];
  };
in
{
  inherit outputs;
  inherit develop;

  packages = {
    inherit (outputs)
      hanjiru
      haskell-like
      mhc
      mhc-haskell;
    default = outputs.mhc;
  };

  devShells.default = develop;
}
