{ lib, mkDerivation, base } :

mkDerivation {
    pname = "haskell-like";
    version = "0";
    src = ../../code/haskell-like;
    libraryHaskellDepends = [ base ];
    license = lib.licenses.bsd3;
}
