{
  inputs = {
    haskell-nix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskell-nix/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  nixConfig = {
    extra-substituters = [
      "https://cache.iog.io"
    ];
    trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    ];
  };

  outputs =
    {
      self,
      flake-utils,
      haskell-nix,
      nixpkgs,
      ...
    }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (
      system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ haskell-nix.overlay ];
          config.allowUnfree = true;
        };
        project = pkgs.haskell-nix.stackProject {
          src = ./.;
        };
      in
      {
        packages = project.flake'.packages;
        devShells.default = project.shellFor {
          tools = {
            stack = "latest";
            haskell-language-server = "latest";
          };
        };
      }
    );

}
