{ config, lib, pkgs, modulesPath, ... }: {
  imports = [
    "${modulesPath}/installer/scan/not-detected.nix"
    ../users/sonia
    ../modules/secrets.nix
    ../modules/unfree.nix
    ../profiles/boot/efi.nix
    ../profiles/common/host-settings.nix
    ../profiles/printer
    ../profiles/virt
  ];

  # Whether to use git-crypt encrypted secrets directory or use temporary / fake
  # values.
  modules.secrets.enable = true;

  boot = {
    initrd.availableKernelModules = [
      "xhci_pci"
      "thunderbolt"
      "nvme"
      "usbhid"
    ];
    initrd.kernelModules = [ ];
    kernelModules = [ "kvm-intel" ];
    extraModulePackages = [ ];
    kernelPackages = pkgs.linuxPackages_latest;
  };

  fileSystems = {
    "/boot" = {
      device = "/dev/disk/by-uuid/43F8-BFB1";
      fsType = "vfat";
    };

    "/" = {
      device = "/dev/disk/by-uuid/37dd6b69-b07b-43ce-ac81-d0a2dceedc4b";
      fsType = "ext4";
    };
  };

  swapDevices = [ ];

  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
}
