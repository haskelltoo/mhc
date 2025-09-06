{
  haskell,

  name,
  version,
  src,

  inputs ? pkgs: with pkgs; [
    base
  ],

  license ? "",

  ...
}:

haskell.mkDerivation {
  pname = name;
  inherit version;
  inherit src;

  libraryHaskellDepends = inputs haskell;

  inherit license;
}
