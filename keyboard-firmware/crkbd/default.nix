{ stdenv, fetchFromGitHub, writeScript, qmk, makeWrapper }:

let
  flash-crkbd = writeScript "flash-crkbd" ''
    #!/usr/bin/env bash
    cd $1/share
    ${qmk}/bin/qmk flash ./.build/crkbd_rev1_willbush.hex -bl dfu
  '';
in
stdenv.mkDerivation rec {
  name = "crkbd-rev6-firmware";

  src = fetchFromGitHub {
    owner = "qmk";
    repo = "qmk_firmware";
    rev = "4a7d65b9d74af40fd5f92b58aad250f33f1af86a";
    sha256 = "1azdbg16xbvz08392nkwx3pp597djb7xr7xn8x0xjj123jknqbfr";
    # date = 2023-01-05T23:40:53+00:00;
    fetchSubmodules = true;
  };

  buildInputs = [ qmk makeWrapper ];

  prePatch = ''
    mkdir -pv keyboards/crkbd/keymaps/willbush
    cp -rv ${./keymap}/* keyboards/crkbd/keymaps/willbush
  '';

  # Use make instead of qmk tool so we can skip try to use git to determine
  # firmware version.
  buildPhase = "make SKIP_GIT=yes crkbd/rev1:willbush";

  installPhase = ''
    mkdir -p $out/bin
    # qmk flash subcommand only becomes available when it's inside of qmk_firmware
    cp -r . $out/share
    # Wrap the script in order to pass the out path to it.
    makeWrapper ${flash-crkbd} $out/bin/flash-crkbd --add-flags $out
  '';
}
