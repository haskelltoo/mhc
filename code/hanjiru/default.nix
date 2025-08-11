{ lib, mkDerivation, base } :

mkDerivation {
    pname = "hanjiru";
    version = "0";
    src = ./.;
    libraryHaskellDepends = [ base ];
    license = lib.licenses.bsd3;
}
