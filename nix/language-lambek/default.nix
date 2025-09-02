{ lib, mkDerivation, base } :

mkDerivation {
    pname = "language-lambek";
    version = "0";
    src = ../../code/language-lambek;
    libraryHaskellDepends = [ base ];
    license = lib.licenses.bsd3;
}
