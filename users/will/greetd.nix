{ pkgs, ... }:
{
  services.greetd = {
    enable = true;
    vt = 2; # The virtual console (tty) that greetd should use.
    settings = {
      default_session.command = ''
        ${pkgs.greetd.tuigreet}/bin/tuigreet \
          --remember \
          --time \
          --asterisks \
          --user-menu \
          --cmd Hyprland
      '';
    };
  };
  # unlock keyring on login
  security.pam.services.greetd.enableGnomeKeyring = true;
}
