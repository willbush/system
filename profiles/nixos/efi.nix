{
  boot = {
    tmp.cleanOnBoot = true;

    loader = {
      # Menu hidden. Tap space repeatedly at boot to show it.
      timeout = 0;

      efi.canTouchEfiVariables = true;

      # Use the systemd-boot EFI boot loader.
      systemd-boot = {
        enable = true;
        # Fixes a security hole in place for the sake of backwards
        # compatibility. See description in:
        # nixpkgs/nixos/modules/system/boot/loader/systemd-boot/systemd-boot.nix
        editor = false;
        bootCounting.enable = true;
        # The 511M ESP filled up at 30 generations.
        configurationLimit = 20;
      };
    };
  };
}
