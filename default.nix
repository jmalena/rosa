{ mkDerivation, alex, array, base, containers, happy, mtl
, optparse-applicative, process, stdenv
}:
mkDerivation {
  pname = "rosa";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ array base containers mtl ];
  libraryToolDepends = [ alex happy ];
  executableHaskellDepends = [ base optparse-applicative process ];
  license = stdenv.lib.licenses.bsd3;
}
