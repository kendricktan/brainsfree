{ mkDerivation, Cabal, cabal-install, stdenv, base, containers, mtl, free
} : mkDerivation {
  pname = "brainsfree";
  version = "0.0.1";
  src = ./.;
  buildTools = [
    cabal-install
  ];
  setupHaskellDepends = [ base Cabal ];
  libraryHaskellDepends = [
    base containers mtl free
  ];
  testHaskellDepends = [
    base
  ];
  homepage = "https://github.com/kendricktan/brainsfree";
  description = "Brainfuck Intepreter using Free-Monads";
  license = stdenv.lib.licenses.mit;
}
