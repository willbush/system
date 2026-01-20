{ pkgs, ... }:
{
  services.greetd = {
    enable = true;
    settings = {
      default_session.command = ''
        ${pkgs.tuigreet}/bin/tuigreet \
          --remember \
          --time \
          --asterisks \
          --user-menu \
          --cmd start-hyprland
      '';
    };
  };
  # unlock keyring on login
  security.pam.services.greetd.enableGnomeKeyring = true;
}
