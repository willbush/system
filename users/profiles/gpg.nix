{
  programs.gpg.enable = true;

  services = {
    gpg-agent = {
      enable = true;
      defaultCacheTtl = 18000;
      defaultCacheTtlSsh = 18000;
    };
  };
}
