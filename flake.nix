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
        # nix build
        packages.default = rosaPkg;

        # nix develop
        devShells.default = hsPkgs.shellFor {
          packages = p: [ rosaPkg ];
          nativeBuildInputs = with pkgs; [
            cabal-install
            ghciwatch
          ];
          shellHook = ''
            echo "Welcome to the Rosa compiler development environment!"
            echo "  ➤ Use 'cabal' to build, test, and run the project."
            echo "  ➤ Use 'dev-watch' for live reloading with ghciwatch."
            alias dev-watch='ghciwatch --command "cabal repl"'
          '';
        };
      }
    );
}
