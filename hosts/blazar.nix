{
  pkgs,
  lib,
  modulesPath,
  config,
  ...
}:
{
  imports = [
    (modulesPath + "/installer/scan/not-detected.nix")
    ../modules/secrets.nix
    ../modules/unfree.nix
    ../persist.nix
    ../profiles/efi.nix
    ../profiles/host-settings.nix
    ../profiles/printer
    ../users/will
  ];

  # Whether to use git-crypt encrypted secrets directory or use temporary / fake
  # values.
  modules.secrets.enable = true;

  services.hardware.openrgb.enable = true;
  services.hardware.openrgb.motherboard = "amd";

  # Generated hardware configuration below:
  boot.initrd.availableKernelModules = [
    "nvme"
    "xhci_pci"
    "ahci"
    "usbhid"
    "usb_storage"
    "sd_mod"
  ];
  boot.initrd.kernelModules = [ "amdgpu" ];
  boot.kernelModules = [ "kvm-amd" ];
  boot.extraModulePackages = [ ];
  boot.kernelPackages = pkgs.linuxPackages_latest;
  # Need this for running WSL2 inside a VM running Windows
  # https://www.linux-kvm.org/page/Nested_Guests
  # https://docs.fedoraproject.org/en-US/quick-docs/using-nested-virtualization-in-kvm/
  boot.extraModprobeConfig = "options kvm-amd nested=1";

  fileSystems."/" = {
    device = "none";
    fsType = "tmpfs";
    options = [
      "defaults"
      "size=25%"
      "mode=755"
    ];
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/7198-4C39";
    fsType = "vfat";
    options = [ "umask=0077" ];
  };

  fileSystems."/nix" = {
    device = "/dev/disk/by-uuid/32caa2ef-8183-450b-a906-e0bd3d03f1e6";
    fsType = "ext4";
  };

  boot.initrd.luks.devices."crypted".device = "/dev/disk/by-uuid/ee9aa93f-df8c-4a85-a0a9-49a4bae51d5a";

  swapDevices = [
    {
      device = "/dev/disk/by-partuuid/1dc4dd53-b7e6-4712-b382-58e30592b3e0";
      randomEncryption.enable = true;
    }
  ];

  # Enables DHCP on each ethernet and wireless interface. In case of scripted networking
  # (the default) this is the recommended approach. When using systemd-networkd it's
  # still possible to use this option, but it's recommended to use it in conjunction
  # with explicit per-interface declarations with `networking.interfaces.<interface>.useDHCP`.
  networking.useDHCP = lib.mkDefault true;

  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
  hardware.cpu.amd.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
}
