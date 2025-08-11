{
    description = "Montreal Haskell Compiler";

    inputs = {
        flake-parts.url = "github:hercules-ci/flake-parts";
        haskell.url = "github:exclusive-and/haskell-nix";
        nixpkgs.url = "nixpkgs/nixos-25.05";
    };

    outputs = { flake-parts, haskell, ... } @ inputs:
        flake-parts.lib.mkFlake { inherit inputs; } {
            imports = [ haskell.flakeModule ];

            perSystem = import ./.;
            
            systems = [ "x86_64-linux" ];
        };
}
