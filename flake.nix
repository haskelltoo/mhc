{
  description = "Montreal Haskell Compiler";

  inputs.nixpkgs.url = "nixpkgs/nixos-25.05";

  outputs = {self, nixpkgs}:
    let
      systems = [
        "x86_64-linux"
      ];

      forAllSystems = nixpkgs.lib.genAttrs systems;

      main = forAllSystems (system:
        import ./. {
          inherit nixpkgs system;
        }
      );
    in
    {
      packages = forAllSystems (system: main.${system}.packages);
      devShells = forAllSystems (system: main.${system}.devShells);
    };
}
