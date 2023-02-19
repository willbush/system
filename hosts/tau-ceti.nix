{ config, lib, modulesPath, ... }: {
  imports = [
    "${modulesPath}/installer/scan/not-detected.nix"
    ../users/will
    ../modules/unfree.nix
    ../profiles/boot/efi.nix
    ../profiles/common/host-settings.nix
    ../profiles/printer
    ../profiles/virt
  ];

  # Open firewall for NFS
  networking.firewall.allowedTCPPorts = [ 2049 ];

  boot = {
    initrd = {
      checkJournalingFS = true; # run fsck for journal file system

      availableKernelModules = [
        "uhci_hcd"
        "ehci_pci"
        "ahci"
        "firewire_ohci"
        "usbhid"
        "usb_storage"
        "sd_mod"
        "sr_mod"
        "sdhci_pci"
      ];
      kernelModules = [ ];
    };

    kernelModules = [ "kvm-intel" "wl" ];

    extraModulePackages = [ config.boot.kernelPackages.broadcom_sta ];
  };

  fileSystems = {
    "/boot" = {
      device = "/dev/disk/by-uuid/841B-2A58";
      fsType = "vfat";
    };

    "/" = {
      device = "/dev/disk/by-uuid/1cc930ea-2e3c-405a-bc7d-543f2f7f0fb3";
      fsType = "ext4";
    };

    "srv/nfs/media" = {
      device = "/tank/media";
      options = [ "bind" ];
    };
  };

  swapDevices =
    [{ device = "/dev/disk/by-uuid/815a345f-d319-4cd6-b7d5-4ca31916b36b"; }];

  nix.settings.max-jobs = lib.mkDefault 4;
  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";

  # Lets me control the backlight via the command line.
  # see https://nixos.wiki/wiki/Backlight
  programs.light.enable = true;

  services = {
    # Enable touchpad support.
    xserver.libinput.enable = true;

    fstrim.enable = true;

    openssh = {
      enable = true;
      settings.passwordAuthentication = false;
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

  modules.unfree.allowList = [ "broadcom-sta" ];
}
