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
    # Needed for mount.cifs command (when manually mounting)
    supportedFilesystems = [ "cifs" ];

    loader.efi.efiSysMountPoint = "/boot/efi";

    initrd = {
      checkJournalingFS = true; # run fsck for journal file system
      availableKernelModules = [ "xhci_pci" "ahci" "usb_storage" "usbhid" "sd_mod" ];
      secrets = {
        "/crypto_keyfile.bin" = null;
      };
    };

    kernelModules = [ "kvm-amd" ];
    extraModulePackages = [ ];

    # Need this for running WSL2 inside a VM running Windows
    # https://www.linux-kvm.org/page/Nested_Guests
    # https://docs.fedoraproject.org/en-US/quick-docs/using-nested-virtualization-in-kvm/
    extraModprobeConfig = "options kvm-amd nested=1";
  };

  fileSystems."/" =
    {
      device = "/dev/disk/by-uuid/e8a78c50-7cc9-4d5f-bd16-12f4d7b27bc5";
      fsType = "ext4";
    };

  boot.initrd.luks.devices."luks-acd1029d-fdd2-496f-922d-ea04379a1b17".device = "/dev/disk/by-uuid/acd1029d-fdd2-496f-922d-ea04379a1b17";

  fileSystems."/boot/efi" =
    {
      device = "/dev/disk/by-uuid/7C41-AC57";
      fsType = "vfat";
    };

  swapDevices = [ ];

  nix.settings.max-jobs = lib.mkDefault 16;

  hardware = {
    nvidia.package = config.boot.kernelPackages.nvidiaPackages.beta;
    opengl.enable = true;
    cpu.amd.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
  };

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

    # needed for globalprotect-openconnect to work
    globalprotect.enable = true;

    samba = {
      enable = true;
      openFirewall = true;
      nsswins = true;
    };
  };

  modules.unfree.allowList = [ "nvidia-x11" "nvidia-settings" ];
}
