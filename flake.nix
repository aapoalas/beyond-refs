{
  description = "Beyond Refs";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = inputs @ { self, ... }:
    inputs.flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import inputs.nixpkgs { inherit system; };

        # Temporarily pull the fix for https://github.com/Michael-F-Bryan/mdbook-linkcheck/pull/98
        mdbook-linkcheck = with pkgs; rustPlatform.buildRustPackage {
          pname = "mdbook-linkcheck";
          version = "PR-98";

          src = fetchFromGitHub {
            owner = "schilkp";
            repo = "mdbook-linkcheck";
            rev = "ed981be6ded11562e604fff290ae4c08f1c419c5";
            sha256 = "sha256-GTVWc/vkqY9Hml2fmm3iCHOzd/HPP1i/8NIIjFqGGbQ=";
          };

          cargoHash = "sha256-+73aI/jt5mu6dR6PR9Q08hPdOsWukb/z9crIdMMeF7U=";

          buildInputs = if stdenv.hostPlatform.isDarwin then [ Security ] else [ openssl ];
          nativeBuildInputs = lib.optionals (!stdenv.hostPlatform.isDarwin) [ pkg-config ];

          OPENSSL_NO_VENDOR = 1;
          doCheck = false;
        };
      in
      {
        devShells.default = pkgs.mkShell {
          packages = [
            pkgs.mdbook
            mdbook-linkcheck
          ];
        };
      });
}
