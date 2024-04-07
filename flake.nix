{
  description = "elm-dev flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  };

  outputs = { self, nixpkgs }:
    let
      pkgs = import nixpkgs { system = "aarch64-darwin"; };

    in {
      devShell.aarch64-darwin = pkgs.mkShell {
        buildInputs = [
          elmPackages.elm
          elmPackages.elm-format
          elmPackages.elm-test-rs
          elmPackages.elm-review
          # LTS Node
          nodejs-18_x
          # For the VSCode client
          yarn
          # Needed for building GHC
          llvmPackages_13.llvm
          # GHC
          stack
          # Other
          git
        ];
      };
    };
}
 
