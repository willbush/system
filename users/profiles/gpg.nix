{
  programs.gpg.enable = true;

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
