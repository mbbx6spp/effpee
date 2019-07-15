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
              xstring framed float;
  };

  # Add development tools on top of the package dependencies since the package build shouldn't depend on dev tools
  haskellDevEnv = haskellPkgs.ghcWithPackages (p: with p; [
    ghcid
    cabal-install
    stylish-haskell
    stylish-cabal
    hiePkgs.hie84
    doctest
    hlint
    hoogle
    hasktags
    hakyll
  ]);
in haskellPkgs.shellFor {
  packages = p: with p; [ effpee ];
  buildInputs =with pkgs; [
    cabal2nix
    bats
    haskellDevEnv
    texliveEnv
    pythonPackages.pygments
    graphviz
    rubyEnv.wrappedRuby
    bundix
    geckodriver
  ];
  withHoogle = true;
}
