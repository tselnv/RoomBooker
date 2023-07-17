{ withHoogle ? false, forceShell ? false }:
# A tutorial on Nix, and how you can use 'developPackage' to override
# dependencies:
#   https://www.srid.ca/1948201.html
let
  # You can get a newer ref by looking under "nixpkgs-unstable" in https://status.nixos.org/
  nixpkgsRev = "4ecab3273592f27479a583fb6d975d4aba3486fe";
  # nixpkgsSha = "sha256:162dywda2dvfj1248afxc45kcrg83appjd0nmdb541hl7rnncf02";
  # We are using the default compiler (8.8 as of this writing) in nixpkgs.
  # To override, set it to for example: pkgs.haskell.packages.ghc865
  compiler = pkgs.haskellPackages;
  pkgs = import (builtins.fetchTarball {
    url = "https://github.com/nixos/nixpkgs/archive/${nixpkgsRev}.tar.gz";
    # sha256 = nixpkgsSha;
  }) {} ;


in
  pkgs.stdenv.mkDerivation {
    name = "env";
    buildInputs =  [
      compiler.stack
      compiler.cabal-install
      compiler.ghcid
      compiler.haskell-language-server
      compiler.ghcide
      pkgs.ormolu
      pkgs.hpack
      compiler.ghc
      pkgs.haskellPackages.record-dot-preprocessor
      pkgs.zlib

      pkgs.haskellPackages.telegram-bot-api
      pkgs.haskellPackages.telegram-bot-simple
      pkgs.haskellPackages.postgresql-libpq
      # pkgs.libpqxx
      pkgs.libpqxx_6
      # pkgs.haskellPackages.libpq
    ];

}

# To run HLS:
# stack new MyProgram
# rm Setup.hs 
# rm stack.yaml 
# rm MyProgram.cabal 
# hpack
# gen-hie > hie.yaml
# cabal build
