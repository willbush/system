{ lib, pkgs, stdenv, autoPatchelfHook, syncthingEnabled ? false }:

lib.mkIf syncthingEnabled
  (stdenv.mkDerivation rec {
    pname = "kubectl-vsphere";
    version = "1.0.0";

    # Proprietary binary blob not available for public download
    src = /home/will/sync/work/vsphere-plugin.zip;

    # Requires patchelf to patch the binary to run.
    nativeBuildInputs = [ autoPatchelfHook pkgs.unzip ];

    installPhase = ''
      unzip $src
      mkdir -p $out/bin
      mv ./bin/kubectl-vsphere $out/bin/
    '';
  })
