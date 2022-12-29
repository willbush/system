{ lib, modulesPath, config, ... }: {
  imports = [
    "${modulesPath}/installer/scan/not-detected.nix"
    ../users/will
    ../modules/unfree.nix
    ../profiles/boot/efi.nix
    ../profiles/common/host-settings.nix
    ../profiles/virt
  ];

  boot = {
    loader.efi.efiSysMountPoint = "/boot/efi";

    initrd = {
      checkJournalingFS = true; # run fsck for journal file system
      availableKernelModules = [ "xhci_pci" "ahci" "nvme" "usbhid" "usb_storage" "sd_mod" ];
      kernelModules = [ "amdgpu" ];
      secrets = {
        "/crypto_keyfile.bin" = null;
      };
    };

    kernelModules = [ "kvm-intel" ];
    # Need this for running WSL2 inside a VM running Windows
    # https://www.linux-kvm.org/page/Nested_Guests
    # https://docs.fedoraproject.org/en-US/quick-docs/using-nested-virtualization-in-kvm/
    extraModprobeConfig = "options kvm-amd nested=1";
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
    };

  swapDevices = [ ];

  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
  hardware.cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;

  services = {
    xserver = {
      videoDrivers = [ "amdgpu" ];
    };
    fstrim.enable = true;
  };
}
