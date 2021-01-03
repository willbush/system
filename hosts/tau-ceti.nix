{ config, lib, modulesPath, ... }: {
  imports = [ "${modulesPath}/installer/scan/not-detected.nix" ../users/will ];

  networking = {
    hostName = "tau-ceti";

    # Open firewall for NFS
    firewall.allowedTCPPorts = [ 2049 ];
  };

  services = {
    # Enable touchpad support.
    xserver.libinput.enable = true;

    fstrim.enable = true;

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

  boot.initrd.availableKernelModules = [
    "uhci_hcd"
    "ehci_pci"
    "ahci"
    "firewire_ohci"
    "usbhid"
    "sd_mod"
    "sr_mod"
    "sdhci_pci"
  ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ "kvm-intel" "wl" ];
  boot.extraModulePackages = [ config.boot.kernelPackages.broadcom_sta ];

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/237f2114-bb54-4b4e-bccc-9c2038f8f028";
    fsType = "ext4";
  };

  boot.initrd.luks.devices."crypted".device =
    "/dev/disk/by-uuid/9c07d830-f972-427f-98bc-1d5738f5515d";

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/1BC9-9CDC";
    fsType = "vfat";
  };

  swapDevices =
    [{ device = "/dev/disk/by-uuid/bae7570e-09d9-4c4d-9794-37ea595e88cb"; }];

  nix.maxJobs = lib.mkDefault 4;
  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
}
