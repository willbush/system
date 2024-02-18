{ config, lib, modulesPath, ... }: {
  imports = [
    "${modulesPath}/installer/scan/not-detected.nix"
    ../modules/secrets.nix
    ../modules/unfree.nix
    ../profiles/efi.nix
    ../profiles/host-settings.nix
    ../profiles/printer
    ../users/sonia
  ];

  # Whether to use git-crypt encrypted secrets directory or use temporary / fake
  # values.
  modules.secrets.enable = true;

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
      device = "/dev/disk/by-uuid/7147-E657";
      fsType = "vfat";
    };

    "/" = {
      device = "/dev/disk/by-uuid/1083beae-939d-4cea-9c6b-fcd34e60d6ad";
      fsType = "ext4";
    };
  };

  swapDevices =
    [{ device = "/dev/disk/by-uuid/5ed331c6-253f-4c88-8651-62563df26913"; }];

  nix.settings.max-jobs = lib.mkDefault 4;
  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";

  # Lets me control the backlight via the command line.
  # see https://nixos.wiki/wiki/Backlight
  programs.light.enable = true;

  services = {
    # Enable touchpad support.
    xserver.libinput.enable = true;

    fstrim.enable = true;
  };

  modules.unfree.allowList = [ "broadcom-sta" ];
}
