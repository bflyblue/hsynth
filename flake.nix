{
  description = "hsynth";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-24.11";
  };

  outputs =
    { self, nixpkgs, ... }:
    let
      supportedSystems = [ "x86_64-linux" "aarch64-darwin" ];
      forallSystems =
        f:
        nixpkgs.lib.genAttrs supportedSystems (
          system:
          f (rec {
            inherit system;
            pkgs = nixpkgsFor system;
            haskellPackages = hpkgsFor system pkgs;
          })
        );
      nixpkgsFor = system: import nixpkgs { inherit system; };
      hpkgsFor =
        system: pkgs:
        with pkgs.haskell.lib;
        pkgs.haskell.packages.ghc98.override {
          overrides = self: super: {
        }; };
    in
    {
      packages = forallSystems (
        {
          system,
          pkgs,
          haskellPackages,
        }:
        {
          hsynth = haskellPackages.callCabal2nix "hsynth" ./. { };
          default = self.packages.${system}.hsynth;
        }
      );
      devShells = forallSystems (
        {
          system,
          pkgs,
          haskellPackages,
        }:
        {
          hsynth = haskellPackages.shellFor {
            packages = p: [ self.packages.${system}.hsynth ];
            buildInputs = with haskellPackages; [
              cabal-install
              haskell-language-server
            ];
            withHoogle = true;
          };
          default = self.devShells.${system}.hsynth;
        }
      );
    };
}
