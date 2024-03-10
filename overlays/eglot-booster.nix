{
  trivialBuild,
  fetchFromGitHub,
  jsonrpc,
  eglot
}:

trivialBuild rec {
  pname = "eglot-booster";
  version = "main-17-02-2024";

  src = fetchFromGitHub {
    owner = "jdtsmith";
    repo = "eglot-booster";
    rev = "e79dea640356eb4a8ed9df3808fe73c7c6db4cc4";
    sha256 = "sha256-ybNqMHCGjzT2+4OfywS7hNw551kIzwI3QqC8tU/GsQI=";
  };

  packageRequires = [ jsonrpc eglot ];
}
