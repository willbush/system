{ lib, modulesPath, ... }: {
  imports = [
    "${modulesPath}/installer/scan/not-detected.nix"
    ../users/will
    ../profiles/boot/efi.nix
    ../profiles/printer
    ../profiles/virt
  ];

  networking = {
    hostName = "betelgeuse";
    useDHCP = false;
  };

  time.timeZone = "America/Chicago";

  sound.enable = true;
  hardware.pulseaudio.enable = true;

  boot = {
    # Needed for mount.nfs command (when manually mounting)
    supportedFilesystems = [ "nfs" ];

    initrd = {
      checkJournalingFS = true; # run fsck for journal file system

      availableKernelModules =
        [ "xhci_pci" "ahci" "usb_storage" "usbhid" "sd_mod" ];
    };

    kernelModules = [ "kvm-amd" ];
    extraModulePackages = [ ];
  };

  fileSystems = {
    "/boot" = {
      device = "/dev/disk/by-uuid/B8DE-034A";
      fsType = "vfat";
    };
    "/" = {
      device = "/dev/disk/by-uuid/5239e314-1900-4793-9b5a-5f84657405d9";
      fsType = "ext4";
    };
    "/mnt/media" = {
      device = "tau-ceti:/media";
      fsType = "nfs";
      options = [
        "x-systemd.automount"
        "noauto"
        "x-systemd.idle-timeout=600" # disconnects after 10 minutes (i.e. 600 seconds)
      ];
    };
  };

  swapDevices =
    [{ device = "/dev/disk/by-uuid/fa3f26dd-a24a-4710-ad3f-1c05c9fb5341"; }];

  nix.maxJobs = lib.mkDefault 16;

  services = {
    xserver.videoDrivers = [ "nvidiaBeta" ];
    fstrim.enable = true;
  };
}
