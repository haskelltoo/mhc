{
  pkgs
, cabal-install
, ghc
, haskell
}:

{
  name
, version
, src
, buildInputs ? hackage: [
    hackage.base
  ]
, license ? ""
}:

haskell.mkDerivation {
  pname = name;
  inherit version;
  inherit src;

  libraryHaskellDepends = buildInputs haskell;

  inherit license;
}
