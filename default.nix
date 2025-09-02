{ pkgs, ... }:

{
    haskell.default = {
        hackage = pkgs.haskell.packages.ghc9101;

        packages = {
            hanjiru.source = ./nix/hanjiru;
            lambek.source = ./nix/language-lambek;
            mhc.source = ./nix/mhc;
            mhc-haskell.source = ./nix/mhc-haskell;
        };
    };
}
