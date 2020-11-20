{
  imports = [ ./hardware-configuration.nix ../common-configuration.nix ];

  networking.hostName = "tau-ceti";

  services = {
    # Enable touchpad support.
    xserver.libinput.enable = true;

    openssh = {
      enable = true;
      passwordAuthentication = false;
      ports = [ 16596 ];
    };

    nfs.server = {
      enable = true;
      exports = ''
        /srv/nfs       192.168.1.90(rw,sync,crossmnt,fsid=0,no_subtree_check)
        /srv/nfs/media 192.168.1.90(rw,sync,no_root_squash,no_subtree_check)
      '';
    };
  };

  fileSystems = {
    "srv/nfs/media" = {
      device = "/tank/media";
      options = [ "bind" ];
    };
  };

  # Open firewall for NFS
  networking.firewall.allowedTCPPorts = [
    2049
  ];
}
