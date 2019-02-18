let
  pkgs = import ./nixpkgs.nix;
  inherit (pkgs) stdenv callPackage lib;
  inherit (pkgs) texlive;

  appRoot = builtins.toPath ./.;
  pname = "bla";
  version = "0.1.0";

  hiePkgs = import (builtins.fetchGit {
    url = https://github.com/domenkozar/hie-nix.git;
    ref = "master";
    rev = "19f47e0bf2e2f1a793bf87d64bf8266062f422b1";
  }) { };

  haskellPkgs = pkgs.haskell.packages.ghc844.extend(self: super: {
    effpee = self.callPackage ./package.nix {};
  });

  # Add development tools on top of the package dependencies since the package build shouldn't depend on dev tools
  haskellDevEnv = haskellPkgs.ghcWithPackages (p: with p; [
    ghcid
    cabal-install
    stylish-haskell
    stylish-cabal
    hiePkgs.hie84
    hlint
    hoogle
    hasktags
  ]);
in haskellPkgs.shellFor {
  packages = p: with p; [ effpee ];
  buildInputs =with pkgs; [
    cabal2nix
    bats
    haskellDevEnv
  ];
  withHoogle = true;
}
