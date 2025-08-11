{ lib, mkDerivation, base } :

mkDerivation {
    pname = "mhc";
    version = "0";
    src = ./.;
    libraryHaskellDepends = [ base ];
    license = lib.licenses.bsd3;
}
