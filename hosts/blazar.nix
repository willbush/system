{
  pkgs,
  lib,
  modulesPath,
  config,
  inputs,
  ...
}:
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

  # UMC204HD probe loop since 7.2.10. Rebuilds only sound/usb so the kernel
  # stays cached.
  # TODO: drop once this fix lands in stable (the build fails then):
  # https://lore.kernel.org/all/20261019102347.2425406-1-tiwai@suse.de/
  boot.extraModulePackages =
    let
      inherit (config.boot.kernelPackages) kernel kernelModuleMakeFlags;
    in
    [
      (kernel.stdenv.mkDerivation {
        pname = "snd-usb-audio-behringer";
        inherit (kernel) version src;
        patches = [ ./patches/behringer-boot-quirk-skip-umc.patch ];
        nativeBuildInputs = kernel.moduleBuildDependencies;
        makeFlags = kernelModuleMakeFlags ++ [
          "-C"
          "${kernel.dev}/lib/modules/${kernel.modDirVersion}/build"
          "M=$(PWD)/sound/usb"
          "INSTALL_MOD_PATH=$(out)"
        ];
        installFlags = [ "INSTALL_MOD_STRIP=1" ];
        enableParallelBuilding = true;
        buildFlags = [ "modules" ];
        installTargets = [ "modules_install" ];
      })
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
  # Keep amdgpu out of the initrd. It blanks the LUKS prompt.
  boot.kernelModules = [ "kvm-amd" ];
  boot.kernelPackages = pkgs.linuxPackages_latest;

  # needed for mounting the NAS
  boot.supportedFilesystems = [ "nfs" ];

  fileSystems."/" = {
    device = "none";
    fsType = "tmpfs";
    options = [
      "defaults"
      "size=25%"
      "mode=755"
    ];
  };

  # compressed swap in RAM as a safety net
  zramSwap.enable = true;

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

  # Enables DHCP on each ethernet and wireless interface. In case of scripted networking
  # (the default) this is the recommended approach. When using systemd-networkd it's
  # still possible to use this option, but it's recommended to use it in conjunction
  # with explicit per-interface declarations with `networking.interfaces.<interface>.useDHCP`.
  networking.useDHCP = lib.mkDefault true;

  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
  hardware.cpu.amd.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
}
