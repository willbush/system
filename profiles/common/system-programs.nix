{
  programs = {
    gnupg.agent = {
      enable = true;
      enableSSHSupport = true;
    };
    # needed for gnome / gtk themes
    dconf.enable = true;
    qt5ct.enable = true;
  };
}
