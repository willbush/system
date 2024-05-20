{
  stdenv,
  fetchFromGitHub,
  writeScript,
  qmk,
  makeWrapper,
}:

let
  flash-planck = writeScript "flash-planck" ''
    #!/usr/bin/env bash
    cd $1/share
    ${qmk}/bin/qmk flash ./.build/planck_rev6_willbush.bin
  '';
in
stdenv.mkDerivation rec {
  name = "planck-rev6-firmware";

  src = fetchFromGitHub {
    owner = "qmk";
    repo = "qmk_firmware";

    rev = "93023511ab0f334bb0394d279b9175d46fe1b3c6";
    sha256 = "6GfwVnHkUARwUXdWAY1ZhI+e3joSLirnVfrCJbHveZg=";
    # date 2024-05-19
    fetchSubmodules = true;
  };

  buildInputs = [
    qmk
    makeWrapper
  ];

  prePatch = ''
    mkdir -pv keyboards/planck/keymaps/willbush
    cp -rv ${./keymap}/* keyboards/planck/keymaps/willbush
  '';

  # Use make instead of qmk tool so we can skip try to use git to determine
  # firmware version.
  buildPhase = "make SKIP_GIT=yes planck/rev6:willbush";

  installPhase = ''
    mkdir -p $out/bin
    # qmk flash subcommand only becomes available when it's inside of qmk_firmware
    cp -r . $out/share
    # Wrap the script in order to pass the out path to it.
    makeWrapper ${flash-planck} $out/bin/flash-planck --add-flags $out
  '';
}
