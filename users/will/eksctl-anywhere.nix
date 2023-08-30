{ stdenv, fetchurl, lib, }:
let
  version = "v0.17.0";
in

stdenv.mkDerivation rec {
  pname = "eksctl-anywhere";
  inherit version;

  src = fetchurl {
    url = "https://anywhere-assets.eks.amazonaws.com/releases/eks-a/45/artifacts/eks-a/${version}/linux/amd64/eksctl-anywhere-${version}-linux-amd64.tar.gz";
    sha256 = "sha256-qo93lHzZyK/3x9GFiaRyKC3dqyNEGpQI5ATPQhc0zdI=";
  };

  unpackPhase = ''
    tar xf $src
  '';

  installPhase = ''
    mkdir -p $out/bin
    cp eksctl-anywhere $out/bin/eksctl-anywhere
  '';

  meta = with lib; {
    description = "CLI tool for creating and managing clusters on EKS Anywhere";
    license = licenses.asl20;
  };
}
