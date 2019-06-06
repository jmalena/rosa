{ mkDerivation, alex, array, base, containers, happy, hspec, mtl
, optparse-applicative, process, QuickCheck, stdenv
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
  testHaskellDepends = [ base hspec QuickCheck ];
  license = stdenv.lib.licenses.bsd3;
}
