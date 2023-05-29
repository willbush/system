{ stdenv, fetchFromGitHub, writeScript, qmk, makeWrapper }:

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
    rev = "42c6920e23d8dace7652155d04310cae60f12e42";
    sha256 = "0cinbmlkvqq6hmawdm9qz7fmv0blay0p91q5z17lrs9chavck00l";
    # date = 2023-05-28T23:27:13+01:00;
    fetchSubmodules = true;
  };

  buildInputs = [ qmk makeWrapper ];

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
