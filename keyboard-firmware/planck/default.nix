{ fetchFromGitHub, dfu-programmer, dfu-util, gcc-arm-embedded-8, makeWrapper
, python3, stdenv, writeScript }:

let
  flash-planck = writeScript "flash-planck" ''
    #!/usr/bin/env bash

    echo "Preparing to flash firmware in..."

    for n in $(seq 5 -1 1);
    do
      echo $n
      sleep 1
    done

    echo "Flashing..."

    # Flashing is typically done using make in the qmk_firmware source directory.
    # The make file (qmk_firmware/tmk_core/chibios.mk) has rules that call the
    # dfu-util. I just wrapped the command with echo to see the arguments it was
    # passing to dfu-util and did "make planck/rev6:willbush:dfu-util" to kick
    # off the typical make build and flash command.

    ${dfu-util}/bin/dfu-util -d 0483:df11 -a 0 -s 0x08000000:leave -D $1/planck_rev6_willbush.bin -v
  '';
in stdenv.mkDerivation rec {
  name = "planck-rev6-firmware";

  src = fetchFromGitHub {
    owner = "qmk";
    repo = "qmk_firmware";
    rev = "10e4487ba368a9e220c96e12a0d17ae729d5c1e1";
    sha256 = "04r8pvgj6zv88h27c76lb5fvj7x3ahhfjc5kimqzbs8sikm5dfs3";
    # date = 2020-12-13T03:19:15+09:00;
    fetchSubmodules = true;
  };

  buildInputs =
    [ dfu-programmer dfu-util gcc-arm-embedded-8 python3 makeWrapper ];

  prePatch = ''
    mkdir -pv keyboards/planck/keymaps/willbush
    cp -rv ${./keymap}/* keyboards/planck/keymaps/willbush
  '';

  buildPhase = "make planck/rev6:willbush";

  installPhase = ''
    mkdir -p $out/bin
    cp .build/planck_rev6_willbush.bin $out/
    # Wrap the script in order to pass the out path to it.
    makeWrapper ${flash-planck} $out/bin/flash-planck --add-flags $out
  '';
}
