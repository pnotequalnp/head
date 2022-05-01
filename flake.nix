{
  inputs = {
    nixpkgs.url = "nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    let
      overlay = final: prev: {
        haskell = prev.haskell // {
          packageOverrides =
            final.lib.composeExtensions prev.haskell.packageOverrides
            (hsFinal: hsPrev: { head = hsFinal.callCabal2nix "head" ./. { }; });
        };
      };
    in flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ overlay ];
        };
        inherit (pkgs.haskell.packages) ghc8107 ghc902 ghc922;
        tools = with ghc922; [ cabal-install fourmolu hlint pkgs.nixfmt ];
        devShell = hs:
          hs.shellFor {
            packages = hsPkgs: with hsPkgs; [ head ];
            nativeBuildInputs = tools ++ [ hs.haskell-language-server ];
          };
      in {
        packages = {
          ghc922 = ghc922.head;
          ghc902 = ghc902.head;
          ghc8107 = ghc8107.head;
          default = ghc922.head;
        };

        apps = rec {
          head = ghc922.head;
          default = head;
        };

        devShells = {
          default = devShell ghc922;
          ghc922 = devShell ghc922;
          ghc902 = devShell ghc902;
          ghc8107 = devShell ghc8107;
          ci = pkgs.mkShell { nativeBuildInputs = tools; };
        };
      }) // {
        overlays.default = overlay;
      };
}
