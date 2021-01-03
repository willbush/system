{ config, pkgs, modulesPath, ... }: {
  imports =
    [ ../users/sonia (modulesPath + "/installer/scan/not-detected.nix") ];

  networking.hostName = "bellatrix";

  boot.loader.grub = {
    enable = true;
    version = 2;
    device = "/dev/sda2";
  };

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  networking.interfaces.enp1s0.useDHCP = true;

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  boot.initrd.availableKernelModules = [
    "ohci_pci"
    "ehci_pci"
    "pata_amd"
    "sata_nv"
    "firewire_ohci"
    "usbhid"
    "usb_storage"
    "sd_mod"
  ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ];

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/56984da0-2290-445c-9038-0ace0bff3ca7";
    fsType = "ext4";
  };

  swapDevices =
    [{ device = "/dev/disk/by-uuid/b91f3312-e433-42ba-ae1f-d9637319b89a"; }];
}
