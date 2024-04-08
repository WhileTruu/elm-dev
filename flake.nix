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
          # pkgs.elmPackages.elm
          # pkgs.elmPackages.elm-format
          # pkgs.elmPackages.elm-test-rs
          # pkgs.elmPackages.elm-review
          # LTS Node
          # pkgs.nodejs-18_x
          # For the VSCode client
          # pkgs.yarn
          # Needed for building GHC
          pkgs.llvmPackages_13.llvm
          # GHC
          pkgs.stack
          pkgs.darwin.apple_sdk.frameworks.Cocoa
          pkgs.darwin.apple_sdk.frameworks.CoreServices
        ];
      };
    };
}
 
