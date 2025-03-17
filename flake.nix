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
            # You can add Haskell-specific overrides here if needed
          }; 
        };
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

            # Development tools
            nativeBuildInputs = with haskellPackages; [
              cabal-install
              haskell-language-server
            ];
            
            # Both build-time and runtime dependencies
            buildInputs = with pkgs; [
              clap
              # Add pkg-config to help find libraries
              pkg-config
            ];
            
            # Ensure CLAP headers are available in C_INCLUDE_PATH
            shellHook = ''
              export C_INCLUDE_PATH=${pkgs.clap}/include:$C_INCLUDE_PATH
              export LIBRARY_PATH=${pkgs.clap}/lib:$LIBRARY_PATH
              export PKG_CONFIG_PATH=${pkgs.clap}/lib/pkgconfig:$PKG_CONFIG_PATH
            '';
            
            # Runtime-only dependencies would go in inputsFrom or propagatedBuildInputs
            # inputsFrom = [ ];
            
            withHoogle = true;
          };
          default = self.devShells.${system}.hsynth;
        }
      );
    };
}
