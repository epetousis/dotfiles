{ prev, final }:
# Based off of https://discourse.nixos.org/t/cannot-get-openai-whisper-cpp-to-work-with-cuda/37965/6
prev.openai-whisper-cpp.overrideAttrs (o: rec {
    version = "1.5.4";
    src = prev.fetchFromGitHub {
      owner = "ggerganov";
      repo = "whisper.cpp";
      rev = "refs/tags/v${version}" ;
      hash = "sha256-9H2Mlua5zx2WNXbz2C5foxIteuBgeCNALdq5bWyhQCk=";
    };
    env = o.env // { WHISPER_CUBLAS = "1"; };
    nativeBuildInputs = (o.nativeBuildInputs or [] ) ++ ( with prev.cudaPackages; [
      cuda_nvcc
    ]);
    buildInputs = (o.buildInputs or [] ) ++ ( with prev.cudaPackages; [
      cudatoolkit
			libcublas
			cuda_cudart
		]);
		postPatch = let
			oldStr = "-lcuda ";
			newStr = "-lcuda -L${prev.cudaPackages.cuda_cudart.lib}/lib/stubs ";
		in ''
			substituteInPlace Makefile \
			--replace '${oldStr}' '${newStr}'
		'';
	})
