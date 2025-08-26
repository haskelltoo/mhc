{ lib, mkDerivation, base } :

mkDerivation {
    pname = "language-lambek";
    version = "0";
    src = ./.;
    libraryHaskellDepends = [ base ];
    license = lib.licenses.bsd3;
}
