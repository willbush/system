{
  pkgs,
  lib,
  modulesPath,
  config,
  inputs,
  ...
}:
let
  # nixGL currently emits evaluation warnings on modern nixpkgs due to legacy
  # `final.system` and `xorg.*` attribute usage; this local import/aliasing keeps
  # behavior the same while silencing those warnings in this system flake.
  # TODO: Remove this compatibility wrapper once nix-community/nixGL replaces
  # xorg.libX11/libxcb/libxshmfence with libx11/libxcb/libxshmfence upstream.
  pkgsForNixGL = pkgs.extend (
    _final: prev: {
      xorg = prev.xorg // {
        libX11 = prev.libx11;
        libxcb = prev.libxcb;
        libxshmfence = prev.libxshmfence;
      };
    }
  );
  nixgl = import "${inputs.nixgl}/default.nix" {
    pkgs = pkgsForNixGL;
    enable32bits = pkgs.stdenv.hostPlatform.system == "x86_64-linux";
    enableIntelX86Extensions = pkgs.stdenv.hostPlatform.system == "x86_64-linux";
  };
in
{
  imports = [
    "${modulesPath}/installer/scan/not-detected.nix"
    ../modules/services/openrgb.nix
    ../modules/unfree.nix
    ../profiles/nixos/efi.nix
    ../profiles/nixos/host-settings.nix
    ../profiles/nixos/nix-settings.nix
    ../profiles/nixos/persist.nix
    ../profiles/nixos/printer.nix
    ../users/will.nix
  ];

  modules.services.openrgb.enable = true;

  environment.systemPackages = [
    nixgl.nixVulkanIntel
  ];

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

  boot.initrd.luks.devices."crypted".device =
    "/dev/disk/by-uuid/ee9aa93f-df8c-4a85-a0a9-49a4bae51d5a";

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
