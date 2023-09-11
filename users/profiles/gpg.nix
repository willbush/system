{ pkgs, ... }:
{
  programs.gpg.enable = true;
  # Use the stable version of GnuPG because for some reason new version freezes
  # Emacs when trying to save using auto-encryption-mode in a gpg file.
  programs.gpg.package = pkgs.stable.gnupg;

  services = {
    gpg-agent = {
      enable = true;
      enableSshSupport = true;

      defaultCacheTtl = 10800; # 3 hours
      defaultCacheTtlSsh = 10800;
      maxCacheTtl = 21600; # 6 hours
      maxCacheTtlSsh = 21600;
    };
  };
}
