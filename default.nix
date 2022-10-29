# nix-env --install --file ./default.nix
let
  nixpkgs = builtins.fetchTarball {
    url =
      "https://github.com/NixOS/nixpkgs/archive/c473cc8714710179df205b153f4e9fa007107ff9.tar.gz";
    sha256 = "0q7rnlp1djxc9ikj89c0ifzihl4wfvri3q1bvi75d2wrz844b4lq";
  };

  config = { allowUnfree = true; };

  ghcVersion = "8107";

  overlay = pkgsNew: pkgsOld: {
    haskell-language-server = pkgsOld.haskell-language-server.override {
      supportedGhcVersions = [ ghcVersion ];
    };

    haskell = pkgsOld.haskell // {
      packages = pkgsOld.haskell.packages // {
        "ghc${ghcVersion}" =
          pkgsOld.haskell.packages."ghc${ghcVersion}".override (old: {
            overrides =
              pkgsNew.lib.composeExtensions (old.overrides or (_: _: { }))
              (haskellPackagesNew: haskellPackagesOld: {
                ormolu = if pkgsNew.system == "aarch64-darwin" then
                  pkgsNew.haskell.lib.overrideCabal haskellPackagesOld.ormolu
                  (_: { enableSeparateBinOutput = false; })
                else
                  haskellPackagesOld.ormolu;
              });
          });
      };
    };
  };

  pkgs = import nixpkgs {
    inherit config;
    overlays = [ overlay ];
  };

in pkgs.mkShell {
  packages = [
    pkgs.ghc
    pkgs.cabal-install
    pkgs.cabal2nix
    pkgs.git
    pkgs.haskell-language-server
    pkgs.stylish-haskell
  ];
}
