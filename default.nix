{ mkDerivation, aeson, ansi-terminal, base, lens, lens-aeson
, stdenv, text, time, wreq, lib
}:
mkDerivation {
  pname = "fipnow";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson ansi-terminal base lens lens-aeson text time wreq
  ];
  license = lib.licenses.bsd3;
}
