let
  pkgs = import <nixpkgs> {};
  haskellPackages = pkgs.haskellPackages.override {
    overrides = self: super: with pkgs.haskell.lib;{
      ghc-syb-utils = dontCheck super.ghc-syb-utils;
  };};
  ghcWithPackages =
    haskellPackages.ghcWithHoogle (g: with g;
    [classy-prelude
     hakyll
     hakyll-favicon hakyll-filestore
     hakyll-ogmarkup hakyll-series
     base
     pandoc pandoc-types
    ]);
in with pkgs;
  runCommand "hakyll-env"
    (with ghcWithPackages.haskellPackages;
      rec
        { ghc = ghcWithPackages;
          shellHook = "eval $(egrep ^export ${ghc}/bin/ghc)";
          buildInputs =
            [ ghcWithPackages zsh ghc-mod hindent cabal-helper
              cabal-install codex stylish-haskell hoogle hlint
              align netcat-openbsd hdevtools];})
  "echo success > $out"
