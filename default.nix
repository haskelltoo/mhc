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

  built = buildHaskell {
    inputs = self: [
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
        name = "haskell-mhc";
        version = "0";
        src = ./code/haskell;

        buildInputs = hackage: with hackage; [
          base
          self.hanjiru
          text
        ];

        license = licenses.bsd3;
      }
      {
        name = "mhc";
        version = "0";
        src = ./code/mhc;

        buildInputs = hackage: with hackage; [
          base
          self.hanjiru
          self.haskell-mhc
          text
        ];

        license = licenses.bsd3;
      }
    ];

    extraTools = hackage: with hackage; [
      cabal-install
      haskell-language-server
    ];

    inherit ghcVersion;
    inherit pkgs;

    haskell = pkgs.haskell.packages.${ghcVersion};
  };

  inherit (pkgs.lib) licenses;
in
{
  inherit (built)
    outputs
    shell;
    
  packages = {
    inherit (built.outputs)
      hanjiru
      haskell-like
      haskell-mhc
      mhc;
    default = built.outputs.mhc;
  };

  devShells.default = built.shell;
}
