{
  description = "Rosa Language Compiler";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        ghcVersion = "ghc96";
        pkgs = import nixpkgs { inherit system; };
        hsPkgs = pkgs.haskell.packages.${ghcVersion};
        rosaPkg = hsPkgs.callCabal2nix "rosa" ./. {};
      in {
        # nix develop
        devShells.default = hsPkgs.shellFor {
          packages = p: [ rosaPkg ];
          buildInputs = with pkgs; [
            # bin
            rosaPkg

            # tools
            cabal-install
            ghcid
          ];
        };

        # nix build
        packages.default = rosaPkg;
      }
    );
}
