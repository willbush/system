{ lib, modulesPath, config, ... }: {
  imports = [
    "${modulesPath}/installer/scan/not-detected.nix"
    ../users/will
    ../modules/unfree.nix
    ../profiles/boot/efi.nix
    ../profiles/common/host-settings.nix
    ../profiles/printer
    ../profiles/virt
  ];

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
      device = "/dev/disk/by-uuid/C231-9103";
      fsType = "vfat";
    };
    "/" = {
      device = "/dev/disk/by-uuid/a036ac4a-9f1a-4fb9-bcac-001f8fc2f5b1";
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
    [{ device = "/dev/disk/by-uuid/9b4fd40c-0435-444b-88b9-10be2a9736a8"; }];

  hardware = {
    nvidia.package = config.boot.kernelPackages.nvidiaPackages.beta;
    # high-resolution display
    video.hidpi.enable = lib.mkDefault true;
  };

  nix.maxJobs = lib.mkDefault 16;

  services = {
    xserver = {
      videoDrivers = [ "nvidia" ];
      # Run my display at 144 hz please. I found this setting by running
      # nvidia-settings. Go do 'X Server Display Configuration' set the
      # resolution to something other than auto and it will let you set the
      # frame rate. Then you can 'Save to X configuration file' where I just
      # save it to a temp location to inspect the output. I found online that
      # metamodes is the one that matters for setting the resolution and frame
      # rate, so I ignored the rest of the generated xorg.conf settings.
      screenSection = ''
        Option "metamodes" "3840x1600_144 +0+0"
      '';
    };
    fstrim.enable = true;
  };

  modules.unfree.allowList = [ "nvidia-x11" "nvidia-settings" "veracrypt" ];
}
