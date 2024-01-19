
{ pkgs }: {
    deps = [
      pkgs.haskellPackages.apply-refact
      pkgs.haskellPackages.hindent
      pkgs.haskellPackages.hlint
      pkgs.haskellPackages.hspec-discover
      pkgs.ghcid.bin
      pkgs.haskellPackages.cabal-install
      pkgs.haskellPackages.ghc
      pkgs.haskell-language-server
  ];
}
