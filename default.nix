{ pkgs, ... }:

{
    haskell.default = {
        hackage = pkgs.haskell.packages.ghc9101;

        packages = {
            hanjiru.source = ./code/hanjiru;
            lambek.source = ./code/lambek;
            mhc.source = ./code/mhc;
            mhc-haskell.source = ./code/mhc-haskell;
        };
    };
}
