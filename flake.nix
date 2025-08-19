{
  description = "Rosa Language Compiler";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        haskellPackages = pkgs.haskell.packages.ghc94;
        rosaPkg = haskellPackages.callCabal2nix "rosa" ./. { };
      in {
        packages.default = rosaPkg;

        devShells.default = haskellPackages.shellFor {
          packages = p: [ self.packages.${system}.default ];

          buildInputs = with pkgs; [
            cabal-install
          ];

          nativeBuildInputs = with haskellPackages; [
            alex
            happy
          ];
        };
      });
}
