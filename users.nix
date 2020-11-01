{ pkgs, ... }:

{
  users.extraUsers.will = {
    isNormalUser = true;
    home = "/home/will";
    uid = 1000;
    extraGroups = [ "wheel" "networkmanager" "docker" "libvirtd" ];
    shell = pkgs.zsh;
  };
}
