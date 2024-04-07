{
  description = "The maeel programming language";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.11";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs { inherit system; };
      in
      {
        formatter = pkgs.alejandra;

        devShells.default = pkgs.mkShell {
          packages = [
            pkgs.rustc
            pkgs.rust-analyzer
            pkgs.rustfmt
          ];
        };

        packages = rec {
          maeel = default;

          default = pkgs.stdenv.mkDerivation {
            pname = "maeel";
            version = "2.0";
            src = ./.;
            nativeBuildInputs = [ pkgs.rustc ];
            installPhase = ''
              mkdir -p "$out/bin"
              cp --reflink=auto ./maeel "$out/bin"
            '';
          };
        };
      }
    );
}
