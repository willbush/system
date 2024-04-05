{ pkgs, ... }:
{

  home.pointerCursor = {
    gtk.enable = true;
    package = pkgs.vanilla-dmz;
    name = "Vanilla-DMZ";
  };

  gtk = {
    enable = true;

    theme.name = "rose-pine";
    theme.package = pkgs.rose-pine-gtk-theme;

    iconTheme.name = "rose-pine";
    iconTheme.package = pkgs.rose-pine-icon-theme;
  };

  qt = {
    enable = true;
    platformTheme = "gtk";
    style = {
      package = pkgs.adwaita-qt;
      name = "adwaita-dark";
    };
  };
}
