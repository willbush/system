{
  boot = {
    tmp.cleanOnBoot = true;

    loader = {
      # Timeout (in seconds) until loader boots the default menu item.
      timeout = 2;

      efi.canTouchEfiVariables = true;

      # Use the systemd-boot EFI boot loader.
      systemd-boot = {
        enable = true;
        # Fixes a security hole in place for the sake of backwards
        # compatibility. See description in:
        # nixpkgs/nixos/modules/system/boot/loader/systemd-boot/systemd-boot.nix
        editor = false;
      };
    };
  };
}
