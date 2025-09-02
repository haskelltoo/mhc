{ pkgs, ... }:

{
    haskell.default = {
        hackage = pkgs.haskell.packages.ghc9101;

        packages = {
            hanjiru.source = ./nix/hanjiru;
            haskell-like.source = ./nix/haskell-like;
            lambek.source = ./nix/language-lambek;
            mhc.source = ./nix/mhc;
            mhc-haskell.source = ./nix/mhc-haskell;
        };
    };
}
