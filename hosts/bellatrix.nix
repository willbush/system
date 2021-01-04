{ config, pkgs, modulesPath, ... }: {
  imports = [
    "${modulesPath}/installer/scan/not-detected.nix"
    ../users/sonia
    (import ../profiles/boot/bios.nix { device = "/dev/sda"; })
    ../profiles/common/host-settings.nix
    ../profiles/printer
  ];

  networking.hostName = "bellatrix";

  boot = {
    initrd = {
      checkJournalingFS = true; # run fsck for journal file system
      availableKernelModules = [
        "ohci_pci"
        "ehci_pci"
        "pata_amd"
        "sata_nv"
        "firewire_ohci"
        "usbhid"
        "usb_storage"
        "sd_mod"
      ];
      kernelModules = [ ];
    };
    kernelModules = [ "kvm-intel" ];
    extraModulePackages = [ ];
  };

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/56984da0-2290-445c-9038-0ace0bff3ca7";
    fsType = "ext4";
  };

  swapDevices =
    [{ device = "/dev/disk/by-uuid/b91f3312-e433-42ba-ae1f-d9637319b89a"; }];

  services.fstrim.enable = true;
}
