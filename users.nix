{ lib, pkgs, ... }:

{
  users = {
    mutableUsers = false;
    users = {
      root.hashedPassword = lib.fileContents ./secrets/hashed-password-root.txt;
      will = {
        isNormalUser = true;
        home = "/home/will";
        hashedPassword = lib.fileContents ./secrets/hashed-password-will.txt;
        shell = pkgs.zsh;
        extraGroups = [ "wheel" "networkmanager" "docker" "libvirtd" ];
        openssh.authorizedKeys.keys = [
          "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIEE2hQsuOQZ3PvM2DdI0vxpaBFoRQpFhGXZmeRq8Srs6 tau-ceti-2020-11-16"
        ];
      };
    };
  };
}
