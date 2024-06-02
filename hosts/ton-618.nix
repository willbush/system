{
  pkgs,
  lib,
  modulesPath,
  config,
  ...
}:
{
  imports = [
    "${modulesPath}/installer/scan/not-detected.nix"
    ../modules/secrets.nix
    ../modules/unfree.nix
    ../profiles/efi.nix
    ../profiles/host-settings.nix
    ../profiles/persist.nix
    ../users/will.nix
  ];

  # Whether to use git-crypt encrypted secrets directory or use temporary / fake
  # values.
  modules.secrets.enable = true;

  boot = {
    # Need this for running WSL2 inside a VM running Windows
    # https://www.linux-kvm.org/page/Nested_Guests
    # https://docs.fedoraproject.org/en-US/quick-docs/using-nested-virtualization-in-kvm/
    extraModprobeConfig = "options kvm-intel nested=1";
    kernelPackages = pkgs.linuxPackages_latest;
  };

  hardware.bluetooth.enable = true;
  services.blueman.enable = true; # provides blueman-manager

  # generated hardware-configuration.nix:

  boot.initrd.availableKernelModules = [
    "xhci_pci"
    "ahci"
    "nvme"
    "usbhid"
    "usb_storage"
    "sd_mod"
  ];
  boot.initrd.kernelModules = [ "amdgpu" ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ];

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
    device = "/dev/disk/by-uuid/E5FE-F2AE";
    fsType = "vfat";
    options = [ "umask=0077" ];
  };

  fileSystems."/nix" = {
    device = "/dev/disk/by-uuid/01af5c79-1394-45d0-8c2c-15032463f896";
    fsType = "ext4";
  };

  boot.initrd.luks.devices."crypted".device = "/dev/disk/by-uuid/4dbc65f8-b320-47cb-b18f-f45f17cc2df0";

  fileSystems."/etc/nixos" = {
    device = "/nix/persist/etc/nixos";
    fsType = "none";
    options = [ "bind" ];
  };

  fileSystems."/var/log" = {
    device = "/nix/persist/var/log";
    fsType = "none";
    options = [ "bind" ];
  };

  swapDevices = [
    {
      device = "/dev/disk/by-partuuid/ec1d7357-3749-4e12-8f95-e0d838a09192";
      randomEncryption.enable = true;
    }
  ];

  # Enables DHCP on each ethernet and wireless interface. In case of scripted networking
  # (the default) this is the recommended approach. When using systemd-networkd it's
  # still possible to use this option, but it's recommended to use it in conjunction
  # with explicit per-interface declarations with `networking.interfaces.<interface>.useDHCP`.
  networking.useDHCP = lib.mkDefault true;
  # networking.interfaces.eno1.useDHCP = lib.mkDefault true;

  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
  hardware.cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
}
