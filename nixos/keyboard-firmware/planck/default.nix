{ dfu-programmer
, dfu-util
, fetchFromGitHub
, gcc-arm-embedded-8
, makeWrapper
, python3
, stdenv
, writeScript
}:

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

    ${dfu-util}/bin/dfu-util -d 0483:df11 -a 0 -s 0x08000000:leave -D $1/planck_rev6_willbush.bin -v
  '';
in stdenv.mkDerivation rec {
  name = "planck-rev6-firmware";

  src = fetchFromGitHub {
    owner = "qmk";
    repo = "qmk_firmware";
    rev = "da86484027f6f5deb33799a67a5053bdaa330f51";
    sha256 = "00d4qndk7sihwpwmzhnb5sdfjkyww0nhl0x3h9720lkmx608a2ld";
    # date = 2019-07-05T11:35:55-07:00;
    fetchSubmodules = true;
  };

  buildInputs = [
    dfu-programmer
    dfu-util
    gcc-arm-embedded-8
    python3
    makeWrapper
  ];

  prePatch = ''
    mkdir -pv keyboards/planck/keymaps/willbush
    cp -rv ${./keymap}/* keyboards/planck/keymaps/willbush
    echo $PWD
  '';

  buildPhase = "make planck/rev6:willbush";

  installPhase = ''
    mkdir -p $out/bin
    cp .build/planck_rev6_willbush.bin $out/
    makeWrapper ${flash-planck} $out/bin/flash-planck --add-flags $out
  '';
}
