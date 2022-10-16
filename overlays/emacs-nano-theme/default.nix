{ lib
, trivialBuild
, fetchFromGitHub
, emacs
, mini-frame
, smex
, ts
}:

trivialBuild rec {
  pname = "nano-theme";
  version = "0.pre+unstable=2022-06-22";

  src = fetchFromGitHub {
    owner = "rougier";
    repo  = pname;
    rev = "185894da71aeab33e52a1bedcde6cea4b148436c";
    hash = "sha256-cjjRwBILeaDkSrAnSxrOHbuZveTSjdJGYIgh01PKDUc=";
  };

  packageRequires = [ mini-frame smex ts ];

  prePatch = ''
    # nano-mu4e requires some non-elpa dependencies that I cbf packaging
    rm nano-session.el nano-mu4e.el
    substituteInPlace nano.el \
      --replace "(require 'nano-session)" "" \
      --replace "(add-to-list 'load-path "/Users/rougier/Documents/GitHub/nano-emacs")" "" \
      --replace "(add-to-list 'load-path ".")" ""
  '';

  meta = {
    homepage = "https://github.com/rougier/nano-theme";
    description = "GNU Emacs / N Λ N O Theme";
    inherit (emacs.meta) platforms;
  };
}
