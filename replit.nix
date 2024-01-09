{ pkgs }: {
    deps = [
        pkgs.haskellPackages.cabal-install
        pkgs.haskellPackages.hspec-discover
        pkgs.haskellPackages.ghc
        pkgs.haskell-language-server
    ];
}
