{ lib, modulesPath, ... }: {
  imports = [
    "${modulesPath}/installer/scan/not-detected.nix"
    ../users/will
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
      availableKernelModules =
        [ "xhci_pci" "ahci" "usb_storage" "usbhid" "sd_mod" ];
      # run fsck for journal file system
      checkJournalingFS = true;
    };

    kernelModules = [ "kvm-amd" ];
    extraModulePackages = [ ];

    cleanTmpDir = true; # cleans all files in /tmp during boot

    loader = {
      # Timeout (in seconds) until loader boots the default menu item.
      timeout = 2;
      # Use the systemd-boot EFI boot loader.
      systemd-boot = {
        enable = true;
        # Limit number of configurations to keep /boot partition from filling
        # up.
        configurationLimit = 10;
        memtest86.enable = true;
        # Fixes a security hole in place for the sake of backwards
        # compatibility. See description in:
        # nixpkgs/nixos/modules/system/boot/loader/systemd-boot/systemd-boot.nix
        editor = false;
      };
      efi.canTouchEfiVariables = true;
    };
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
