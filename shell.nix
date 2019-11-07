{ ghcVersion ? "ghc864"
, nixpkgs ? import ./nixpkgs.nix
}:
let
  inherit (nixpkgs) pkgs;
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

  haskellPkgs = pkgs.haskell.packages."${ghcVersion}".extend(self: super: {
    effpee = super.callPackage ./package.nix {};
    Diff = pkgs.haskell.lib.dontCheck super.Diff;
  });

  rubyEnv = pkgs.bundlerEnv {
    inherit (pkgs) ruby;

    name = "effpee-ruby";
    gemfile   = ./Gemfile;
    lockfile  = ./Gemfile.lock;
    gemset    = ./gemset.nix;
  };

  texliveEnv = texlive.combine {
    inherit (texlive)
              beamer beamertheme-metropolis pgf pgfopts pdfpages pdftools
              listings collection-fontsrecommended collection-mathscience
              collection-xetex fancyvrb fontspec caption tikz-cd fira
              etoolbox trimspaces environ ulem capt-of wrapfig tcolorbox
              booktabs translator minted fvextra upquote lineno ifplatform
              xstring framed float mathtools;
  };

  # Add development tools on top of the package dependencies since the package build shouldn't depend on dev tools
  haskellDevEnv = haskellPkgs.ghcWithPackages (p: with p; [
    ghcid
    cabal-install
    stylish-haskell
    stylish-cabal
    doctest
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
    texliveEnv
    plantuml
    pythonPackages.pygments
    graphviz
    rubyEnv.wrappedRuby
    bundix
    geckodriver
  ];
  withHoogle = true;
}
