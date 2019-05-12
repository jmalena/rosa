{ mkDerivation, alex, array, base, happy, stdenv }:
mkDerivation {
  pname = "rosa";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ array base ];
  libraryToolDepends = [ alex happy ];
  executableHaskellDepends = [ base ];
  license = stdenv.lib.licenses.bsd3;
}
