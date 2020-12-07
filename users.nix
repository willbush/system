{ pkgs, ... }:

{
  users = {
    mutableUsers = false;
    users = {
      root.hashedPassword =
        "$6$zsnB4Yv6a$9JRJdUvKo/7GHyzjfcHwGHrDrjbD/tL0zoJbWfMtguj6cseFGTxa5DRiSCNjQWYCPoMbsTNA1ohcrmDOk4Xba0";
      will = {
        isNormalUser = true;
        home = "/home/will";
        hashedPassword =
          "$6$cCY2ySVkvcS$NB4Q.AqH3O6CKAc8MKYjmmOu6w2cMRc5TktjkWTSKKBoXcwu9cnmfFDbMXEDEgau64jU8h1Rmf/TM6fIna2Gb1";
        shell = pkgs.zsh;
        extraGroups = [ "wheel" "networkmanager" "docker" "libvirtd" ];
        openssh.authorizedKeys.keys = [
          "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIEE2hQsuOQZ3PvM2DdI0vxpaBFoRQpFhGXZmeRq8Srs6 tau-ceti-2020-11-16"
        ];
      };
    };
  };
}
