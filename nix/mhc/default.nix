{ lib, mkDerivation, base } :

mkDerivation {
    pname = "mhc";
    version = "0";
    src = ../../code/mhc;
    libraryHaskellDepends = [ base ];
    license = lib.licenses.bsd3;
}
