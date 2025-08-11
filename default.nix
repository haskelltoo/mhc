{ pkgs, ... }:

{
    haskell.default = {
        hackage = pkgs.haskell.packages.ghc9101;

        packages = {
            hanjiru.source = ./code/hanjiru;
            mhc.source = ./code/mhc;
            mhc-haskell.source = ./code/mhc-haskell;
        };
    };
}
