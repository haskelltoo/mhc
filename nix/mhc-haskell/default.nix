{ lib, mkDerivation, base } :

mkDerivation {
    pname = "mhc-haskell";
    version = "0";
    src = ../../code/mhc-haskell;
    libraryHaskellDepends = [ base ];
    license = lib.licenses.bsd3;
}
