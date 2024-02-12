{ pkgs, lib, modulesPath, config, ... }: {
  imports = [
    "${modulesPath}/installer/scan/not-detected.nix"
    ../users/will
    ../modules/secrets.nix
    ../modules/unfree.nix
    ../profiles/efi.nix
    ../profiles/common/host-settings.nix
    ../profiles/virt
  ];

  # Whether to use git-crypt encrypted secrets directory or use temporary / fake
  # values.
  modules.secrets.enable = true;

  boot = {
    loader.efi.efiSysMountPoint = "/boot/efi";

    initrd = {
      checkJournalingFS = true; # run fsck for journal file system
      availableKernelModules = [ "xhci_pci" "ahci" "nvme" "usbhid" "usb_storage" "sd_mod" ];
      kernelModules = [ "amdgpu" ];
      secrets."/crypto_keyfile.bin" = null;
    };

    kernelModules = [ "kvm-intel" ];
    # Need this for running WSL2 inside a VM running Windows
    # https://www.linux-kvm.org/page/Nested_Guests
    # https://docs.fedoraproject.org/en-US/quick-docs/using-nested-virtualization-in-kvm/
    extraModprobeConfig = "options kvm-amd nested=1";
    kernelPackages = pkgs.linuxPackages_latest;
  };

  fileSystems."/" =
    {
      device = "/dev/disk/by-uuid/af889332-0e49-4822-adf8-4832ec485341";
      fsType = "ext4";
    };

  boot.initrd.luks.devices."luks-d859f723-a2e7-41b9-ba03-af39da55e927".device = "/dev/disk/by-uuid/d859f723-a2e7-41b9-ba03-af39da55e927";

  fileSystems."/boot/efi" =
    {
      device = "/dev/disk/by-uuid/136A-4599";
      fsType = "vfat";
      options = [ "umask=0077" ];
    };

  swapDevices = [ ];

  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";

  hardware = {
    bluetooth.enable = true;
    opengl.enable = true; # used for libvirt 3D acceleration
    cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
  };

  services = {
    blueman.enable = true; # provides blueman-manager
    xserver = {
      videoDrivers = [ "amdgpu" ];
    };
    fstrim.enable = true;
  };
}
