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

let
  deps = buildInputs haskell;
in
haskell.mkDerivation {
  pname = name;
  inherit version;
  inherit src;

  libraryHaskellDepends = deps;

  inherit license;
}
