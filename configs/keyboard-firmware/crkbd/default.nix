{
  stdenv,
  fetchFromGitHub,
  writeScript,
  qmk,
  makeWrapper,
}:

let
  flash-crkbd = writeScript "flash-crkbd" ''
    #!/usr/bin/env bash
    cd $1/share
    ${qmk}/bin/qmk flash ./.build/crkbd_rev1_willbush.hex -bl dfu
  '';
in
stdenv.mkDerivation {
  name = "crkbd-rev6-firmware";

  src = fetchFromGitHub {
    owner = "qmk";
    repo = "qmk_firmware";
    rev = "e6a898e3eea3521d3f0497ee26937d5269d78fb7";
    sha256 = "sha256-kc6IDmPiiZDSdVPhCuyM9I9mnXTSnyK06JZ4SZdRtrM=";
    # date 2024-08-24
    fetchSubmodules = true;
  };

  buildInputs = [
    qmk
    makeWrapper
  ];

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
